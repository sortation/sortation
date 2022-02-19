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
import Database.Persist.Pagination
import Database.Persist.Sql hiding (Sql)
import Database.Persist.Sqlite (withSqliteConn, withSqlitePool)
import Sortation.Persistent

(#.) ::
  (PersistEntity a, PersistField b) =>
  SqlExpr (Entity a) ->
  EntityField a b ->
  SqlExpr (Value b)
(#.) = (Esqueleto.^.)

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

stream ::
  ( [Sql, IOE] :>> es
  , PersistEntitySql a
  ) =>
  EntityField a (Key a) ->
  [Filter a] ->
  ConduitT i (Entity a) (Eff es) ()
stream field filters =
  transPipe sql $ streamEntities
    filters
    field
    (PageSize 1)
    Ascend
    (Range Nothing Nothing)
