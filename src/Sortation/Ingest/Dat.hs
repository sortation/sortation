module Sortation.Ingest.Dat where

import Cleff
import Cleff.Optics
import Cleff.Path
import Cleff.Reader
import Cleff.Sql
import Control.Monad
import Control.Monad.Trans
import Data.Conduit
import Data.Conduit.Combinators qualified as Conduit
import Data.Foldable
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Set qualified as Set
import Data.Vector (Vector)
import Database.Persist.Class
import Optics
import Sortation.Ingest.Config
import Sortation.Persistent qualified as Persistent
import System.Path ((</>))
import System.Path qualified as Path
import Text.Dat qualified as Dat
import Text.Dat.Parse
import Text.XML.Stream.Parse qualified as XML

logText :: [Reader Config, IOE] :>> es => Text -> Eff es ()
logText msg = do
  verbose <- peruse @Config #verbose
  when verbose $ liftIO $ Text.putStrLn msg

persistDat ::
  [Reader Config, Sql, IOE] :>> es =>
  ConduitT Dat.Game o (Eff es) ()
persistDat = do
  collectionName <- lift $ peruse @Config #name
  collectionId <- lift $ sql $ insert Persistent.Collection
    { name = collectionName
    , parents = Set.empty
    }
  lift $ logText $ "added " <> collectionName <> " with ID " <> Text.pack (show collectionId)
  Conduit.mapM_ (persistGame collectionId)

persistGame ::
  [Reader Config, Sql, IOE] :>> es =>
  Persistent.CollectionId ->
  Dat.Game ->
  Eff es ()
persistGame collectionId game = do
  gameId <- sql $ insert Persistent.Game
    { parents = Set.singleton collectionId
    , name = game ^. #name
    }
  releaseId <- sql $ insert Persistent.Release
    { parent = gameId
    , region = Nothing
    , language = Nothing
    , version = Nothing
    , date = Nothing
    }
  logText $ "added " <> (game ^. #name) <> " with ID " <> Text.pack (show gameId)
  for_ (game ^. #roms) \rom -> do
    romPath <- gameRomPath game (rom ^. #name)
    persistRom releaseId romPath rom

persistRom ::
  [Reader Config, Sql, IOE] :>> es =>
  Persistent.ReleaseId ->
  Path.AbsFile ->
  Dat.Rom ->
  Eff es ()
persistRom releaseId romPath rom = do
  romId <- sql $ insert Persistent.Rom
    { parent = releaseId
    , name = rom ^. #name
    , path = Text.pack $ Path.toString romPath
    , size = rom ^. #size
    , crc = rom ^. #crc
    , md5 = rom ^. #md5
    , sha1 = rom ^. #sha1
    , lastCheck = Nothing
    }
  logText $ "added " <> (rom ^. #name) <> " with ID " <> Text.pack (show romId)

gameRomPath ::
  [Reader Config, IOE] :>> es =>
  Dat.Game ->
  Text ->
  Eff es Path.AbsFile
gameRomPath game romName = do
  rootDir <- normalize =<< peruse @Config #rootDir
  peruse @Config #hierarchy <&> Path.combine rootDir . \case
    Flat -> relFile romName
    Mixed ->
      if length (game ^. #roms) == 1 then
        relFile romName
      else
        relDir (game ^. #name) </> relFile romName
    Nested -> relDir (game ^. #name) </> relFile romName
