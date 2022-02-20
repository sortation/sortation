module Sortation.Report.Text where

import Cleff
import Cleff.Mask
import Cleff.Sql
import Control.Monad
import Control.Monad.Trans.Resource
import Control.Monad.Trans
import Data.Conduit
import Data.Conduit.Combinators qualified as Conduit
import Data.Foldable
import Data.Function hiding (on)
import Data.Functor
import Data.List qualified as List
import Data.Sequence (Seq, viewr, viewl, ViewR(..), ViewL(..))
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Database.Esqueleto.Experimental hiding (Sql, (^.), (%))
import Database.Esqueleto.Pagination
-- import Database.Persist.Sql hiding (Sql, (==.))
import Optics hiding ((:>), (:<))
import Sortation.Persistent

-- romOutputOptions :: OutputOptions
-- romOutputOptions =
--   defaultOutputOptionsNoColor
--     & #outputOptionsIndentAmount .~ 2
--     -- & #outputOptionsInitialIndent .~ 8

sinkFst ::
  Eq a =>
  (a -> Eff es ()) ->
  ConduitT (a, b) b (Eff es) ()
sinkFst f =
  void $ flip Conduit.mapAccumWhileM Nothing \(x, y) x' ->
    if Just x == x' then
      pure $ Right (x', y)
    else
      f x $> Right (Just x, y)

grouple :: (Foldable f, Eq a) => f (a, b) -> Seq (a, Seq b)
grouple =
  let
    go (viewr -> EmptyR) (x, y) = [(x, [y])]
    go (viewr -> out :> (x', ys)) (x, y) =
      if x == x' then
        out |> (x', ys |> y)
      else
        out |> (x', ys) |> (x, [y])
  in
    foldl' go []

streamRomSets ::
  [Sql, IOE] :>> es =>
  ConduitT () (Entity RomSet) (Eff es) ()
streamRomSets =
  transPipe sql $ streamEntities
    (const (val True))
    RomSetId
    (PageSize 1024)
    Ascend
    (Range Nothing Nothing)

-- streamJoin ::
--   [Sql, IOE] :>> es =>
--   
-- streamJoin = undefined

streamRoms ::
  [Sql, IOE] :>> es =>
  ConduitT (Entity RomSet) (Entity Rom) (Eff es) ()
streamRoms =
  streamJoin @RomSet @RomSetRom @Rom
    (PageSize 1024) (PageSize 1024)
    (#romSet) (view #rom)

streamFiles ::
  [Sql, IOE] :>> es =>
  ConduitT (Entity Rom) (Entity File) (Eff es) ()
streamFiles =
  streamJoin @Rom @RomFile @File
    (PageSize 1024) (PageSize 1024)
    (#rom) (view #file)

fun :: [Sql, IOE] :>> es => Eff es ()
fun = runConduit $ streamRomSets .| streamRoms .| streamFiles .| Conduit.mapM_ (liftIO . putStrLn . show)

-- streamFiles ::
--   [Sql, IOE] :>> es =>
--   ConduitT () _ (Eff es) ()
-- streamFiles =
--   transPipe sql $ selectSource undefined

libraryQuery :: SqlQuery (SqlExpr (Entity RomSet), (SqlExpr (Entity Rom), SqlExpr (Entity File)))
libraryQuery = do
  (romSet :& _ :& rom :& _ :& file) <-
    from $
      table @RomSet
    `innerJoin` table @RomSetRom `on`
      (\(romSet :& romSetRom) -> romSet #. #id ==. romSetRom #. #romSet)
    `innerJoin` table @Rom `on`
      (\(_ :& romSetRom :& rom) -> romSetRom #. #rom ==. rom #. #id)
    `innerJoin` table @RomFile `on`
      (\(_ :& _ :& rom :& romFile) -> rom #. #id ==. romFile #. #rom)
    `innerJoin` table @File `on`
      (\(_ :& _ :& _ :& romFile :& file) -> romFile #. #file ==. file #. #id)
  orderBy [asc (romSet #. #name), asc (rom #. #name), asc (file #. #name)]
  pure (romSet, (rom, file))

selectRomSets ::
  [Sql, IOE] :>> es =>
  Eff es (Seq (Entity RomSet, (Seq (Entity Rom, Seq (Entity File)))))
selectRomSets =
  fmap (fmap (fmap grouple) . grouple) $ sql $ select do
    (romSet :& _ :& rom :& _ :& file) <-
      from $
        table @RomSet
      `innerJoin` table @RomSetRom `on`
        (\(romSet :& romSetRom) -> romSet #. #id ==. romSetRom #. #romSet)
      `innerJoin` table @Rom `on`
        (\(_ :& romSetRom :& rom) -> romSetRom #. #rom ==. rom #. #id)
      `innerJoin` table @RomFile `on`
        (\(_ :& _ :& rom :& romFile) -> rom #. #id ==. romFile #. #rom)
      `innerJoin` table @File `on`
        (\(_ :& _ :& _ :& romFile :& file) -> romFile #. #file ==. file #. #id)
    orderBy [asc (romSet #. #name), asc (rom #. #name), asc (file #. #name)]
    pure (romSet, (rom, file))

-- printRomSets :: [Sql, IOE] :>> es => Eff es ()
-- printRomSets =
--   liftIO . pPrintOpt NoCheckColorTty romOutputOptions =<< selectRomSets

-- printCollections :: [Sql, IOE] :>> es => Eff es ()
-- printCollections =
--   runConduit $ stream CollectionId [] .| Conduit.mapM_ \collection -> do
--     liftIO $ Text.putStrLn $ "COLLECTION: " <> collection ^. #entityVal % #name
--     runConduit $ stream GameId [gameFilter (collection ^. #entityKey)] .| Conduit.mapM_ \game -> do
--       liftIO $ Text.putStrLn $ "  GAME: " <> game ^. #entityVal % #name
--       runConduit $ stream ReleaseId [releaseFilter (game ^. #entityKey)] .| Conduit.mapM_ \release -> do
--         liftIO $ Text.putStrLn $ "    RELEASE: " <> releaseString (release ^. #entityVal)
--         runConduit $ stream RomId [romFilter (release ^. #entityKey)] .| Conduit.mapM_ \rom -> do
--           liftIO $ Text.putStrLn $ "      ROM: " <> rom ^. #entityVal % #name
--           pPrintOpt NoCheckColorTty romOutputOptions rom
