module Cleff.Sql where

import Cleff
import Cleff.Reader
import Control.Monad.Logger
import Control.Monad.IO.Unlift
import Control.Monad.Reader qualified as Mtl
import Control.Monad.Trans.Control
import Data.Conduit
import Data.Conduit.Combinators qualified as Conduit
import Data.Conduit.Concurrent
import Data.Pool
import Data.Set qualified as Set
import Data.Text (Text)
import Database.Esqueleto.Experimental hiding (Sql, (^.))
import Database.Esqueleto.Experimental qualified as Esqueleto
import Database.Esqueleto.Pagination
import Database.Persist.Sqlite (withSqliteConn, withSqlitePool)
import Optics
import Sortation.Persistent

(#.) ::
  (PersistEntity a, PersistField b) =>
  SqlExpr (Entity a) ->
  EntityField a b ->
  SqlExpr (Value b)
(#.) = (Esqueleto.^.)

wildcard :: SqlString s => SqlExpr (Value s)
wildcard = (Esqueleto.%)

data Sql :: Effect where
  Sql :: Mtl.ReaderT SqlBackend m a -> Sql m a

makeEffect ''Sql

runSqlBackend :: SqlBackend -> Eff (Sql : es) ~> Eff es
runSqlBackend backend = interpret \(Sql m) -> toEff $ Mtl.runReaderT m backend

runSilentSqliteConn :: IOE :> es => Text -> Eff (Sql : es) ~> Eff es
runSilentSqliteConn db =
  interpret \(Sql m) -> 
    withRunInIO \unlift -> runNoLoggingT $
      withSqliteConn db
        (liftIO . runSqlConn (liftWith \liftReaderT -> unlift (toEff (liftReaderT m))))

runSilentSqlitePool :: IOE :> es => Text -> Int -> Eff (Sql : es) ~> Eff es
runSilentSqlitePool db connCount =
  interpret \(Sql m) ->
    withRunInIO \unlift -> runNoLoggingT $
      withSqlitePool db connCount \pool ->
        withResource pool
          (liftIO . runSqlConn (liftWith \liftReaderT -> unlift (toEff (liftReaderT m))))
    
type PersistEntitySql a =
  ( PersistEntity a
  , PersistEntityBackend a ~ BaseBackend SqlBackend
  )

-- stream ::
--   ( [Sql, IOE] :>> es
--   , PersistEntitySql a
--   ) =>
--   EntityField a (Key a) ->
--   [Filter a] ->
--   ConduitT i (Entity a) (Eff es) ()
-- stream field filters =
--   transPipe sql $ streamEntities
--     filters
--     field
--     (PageSize 1)
--     Ascend
--     (Range Nothing Nothing)

streamJoin ::
  forall a ab b es.
  ( [Sql, IOE] :>> es
  , PersistEntitySql a
  , PersistEntitySql ab
  , PersistEntitySql b
  ) =>
  PageSize ->
  PageSize ->
  EntityField ab (Key a) ->
  (ab -> Key b) ->
  ConduitT (Entity a) (Entity b) (Eff es) ()
streamJoin zl zr aba abb =
  fuse
    do
      awaitForever \x ->
        transPipe sql $ streamEntities
          (\xy -> xy #. aba ==. val (entityKey x))
          (persistIdField @ab)
          zl
          Ascend
          (Range Nothing Nothing)
    do
      awaitForever \xy ->
        transPipe sql $ streamEntities
          (\y -> val (abb (entityVal xy)) ==. y #. persistIdField)
          (persistIdField @b)
          zr
          Ascend
          (Range Nothing Nothing)
          

-- streamRoms ::
--   [Sql, IOE] :>> es =>
--   ConduitT (Entity RomSet) (Entity Rom) (Eff es) ()
-- streamRoms =
--   fuse
--     do
--       awaitForever \romSet ->
--         transPipe sql $ streamEntities
--           (\romSetRom -> (romSetRom #. #romSet) ==. val (romSet ^. #entityKey))
--           RomSetRomId
--           (PageSize 1)
--           Ascend
--           (Range Nothing Nothing)
--     do
--       awaitForever \romSetRom ->
--         transPipe sql $ streamEntities
--           (\rom -> val (romSetRom ^. #entityVal % #rom) ==. (rom #. #id))
--           RomId
--           (PageSize 1)
--           Ascend
--           (Range Nothing Nothing)
