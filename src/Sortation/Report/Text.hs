module Sortation.Report.Text where

import Cleff
import Cleff.Sql
import Data.Conduit
import Data.Conduit.Combinators qualified as Conduit
import Data.Foldable
import Data.List qualified as List
import Data.Sequence (Seq, viewr, viewl, ViewR(..), ViewL(..))
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Database.Esqueleto.Experimental hiding (Sql, (^.))
import Database.Esqueleto.Pagination
-- import Database.Persist.Sql hiding (Sql, (==.))
import Optics hiding ((:>), (:<))
import Sortation.Persistent
import Text.Pretty.Simple

romOutputOptions :: OutputOptions
romOutputOptions =
  defaultOutputOptionsNoColor
    & #outputOptionsIndentAmount .~ 2
    & #outputOptionsInitialIndent .~ 8

grouple :: (Foldable f, Eq a) => f (a, b) -> Seq (a, Seq b)
grouple =
  let
    -- go out [] = out
    go (viewr -> EmptyR) (x, y) = [(x, [y])]
    go (viewr -> out :> (x', ys)) (x, y) =
      if x == x' then
        out |> (x', ys |> y)
      else
        out |> (x', ys) |> (x, [y])
  in
    foldl' go []

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
    orderBy [ asc (romSet #. #name), asc (rom #. #name), asc (file #. #name) ]
    pure (romSet, (rom, file))

printRomSets :: [Sql, IOE] :>> es => Eff es ()
printRomSets =
  liftIO . pPrintOpt NoCheckColorTty romOutputOptions =<< selectRomSets

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
