module Sortation.Report.Text where

import Cleff
import Cleff.Sql
import Data.Conduit
import Data.Conduit.Combinators qualified as Conduit
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Database.Persist.Sql hiding (Sql)
import Optics
import Sortation.Persistent
import Text.Pretty.Simple

romOutputOptions :: OutputOptions
romOutputOptions =
  defaultOutputOptionsNoColor
    & #outputOptionsIndentAmount .~ 2
    & #outputOptionsInitialIndent .~ 8

printCollections :: [Sql, IOE] :>> es => Eff es ()
printCollections =
  runConduit $ stream CollectionId [] .| Conduit.mapM_ \collection -> do
    liftIO $ Text.putStrLn $ "COLLECTION: " <> collection ^. #entityVal % #name
    runConduit $ stream GameId [gameFilter (collection ^. #entityKey)] .| Conduit.mapM_ \game -> do
      liftIO $ Text.putStrLn $ "  GAME: " <> game ^. #entityVal % #name
      runConduit $ stream ReleaseId [releaseFilter (game ^. #entityKey)] .| Conduit.mapM_ \release -> do
        liftIO $ Text.putStrLn $ "    RELEASE: " <> releaseString (release ^. #entityVal)
        runConduit $ stream RomId [romFilter (release ^. #entityKey)] .| Conduit.mapM_ \rom -> do
          liftIO $ Text.putStrLn $ "      ROM: " <> rom ^. #entityVal % #name
          pPrintOpt NoCheckColorTty romOutputOptions rom
