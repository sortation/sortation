module Sortation.Ingest.Dat where

import Cleff
import Cleff.Mask
import Cleff.Optics
import Cleff.Path
import Cleff.Reader
import Cleff.Sql
import Control.Applicative
import Control.Exception (Exception, throwIO)
import Control.Monad
import Control.Monad.Trans
import Data.Attoparsec.Text
import Data.Conduit
import Data.Conduit.Combinators qualified as Conduit
import Data.Conduit.Combinators qualified as Conduit
import Data.Foldable
import Data.Functor
import Data.IORef
import Data.Maybe
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Vector (Vector)
import Database.Persist.Class
import GHC.Generics
import Optics
import Sortation.Format.Dat qualified as Dat
import Sortation.Format.Dat.Parse
import Sortation.Format.NoIntro qualified as NoIntro
import Sortation.Ingest.Config
import Sortation.Library qualified as Library
import Sortation.Persistent qualified as Persistent
import System.Path ((</>))
import System.Path qualified as Path
import System.Path.IO
import Text.XML.Stream.Parse qualified as XML

ingestDat ::
  [Reader Config, Mask, Sql, IOE] :>> es =>
  Eff es ()
ingestDat = do
  headerRef <- liftIO $ newIORef Nothing
  bracket
    do liftIO . flip openFile ReadMode =<< normalizeFile =<< peruse @Config #datFile
    do liftIO . hClose
    do
      \handle -> runConduit $
        Conduit.sourceHandle handle
          .| XML.parseBytes XML.def
          .| parseDat (liftIO . writeIORef headerRef)
          .| (persistDat =<< liftIO (readIORef headerRef))

logText :: [Reader Config, IOE] :>> es => Text -> Eff es ()
logText msg = do
  verbose <- peruse @Config #verbose
  when verbose $ liftIO $ Text.putStrLn msg

parseGroup :: Text -> Maybe Persistent.Group
parseGroup "No-Intro" = Just Persistent.NoIntro
parseGroup _ = Nothing

groupNamingConvention :: Persistent.Group -> NamingConvention
groupNamingConvention Persistent.NoIntro = NoIntro

persistDat ::
  [Reader Config, Sql, IOE] :>> es =>
  Maybe Dat.Header ->
  ConduitT Dat.Game o (Eff es) ()
persistDat header = do
  Config { .. } <- lift ask
  let
    (romSet, namingConvention) = case header of
      Nothing ->
        ( Persistent.emptyRomSet name rootDir & #datFile .~ Just datFile
        , fromNamingConventionOption Nothing namingConventionOption
        )
      Just Dat.Header { .. } ->
        let group = parseGroup =<< homepage in
          ( Persistent.RomSet
            { directory = rootDir
            , description = Just description
            , version = Just version
            , author = Just author
            , datFile = Just datFile
            , ..
            }
          , fmap groupNamingConvention group
          )
  romSetId <- lift $ sql $ insert romSet
  lift $ logText $ "added " <> name <> " with ID " <> Text.pack (show romSetId)
  Conduit.mapM_ (persistRom romSetId namingConvention)

newtype TitleParsingError = TitleParsingError String
  deriving (Generic, Eq, Ord, Show, Read)
  deriving anyclass Exception

parseDatGame ::
  IOE :> es =>
  Maybe NamingConvention ->
  Dat.Game ->
  Eff es Persistent.Rom
parseDatGame namingConvention Dat.Game { .. } = do
  case namingConvention of
    Nothing -> pure Persistent.Rom
      { regions = Set.empty
      , languages = Set.empty
      , version = Nothing
      , date = Nothing
      , disc = Nothing
      , ..
      }
    Just NoIntro -> do
      NoIntro.Title { .. } <-
        either
          do liftIO . throwIO . TitleParsingError
          do pure
          do parseOnly NoIntro.parseTitle name
      pure Persistent.Rom
        { date = Nothing
        , ..
        }

persistRom ::
  [Reader Config, Sql, IOE] :>> es =>
  Persistent.RomSetId ->
  Maybe NamingConvention ->
  Dat.Game ->
  Eff es ()
persistRom romSetId namingConvention game = do
  romId <- sql . insert =<< parseDatGame namingConvention game
  sql $ insert_ $ Persistent.RomSetRom romSetId romId
  logText $ "added " <> (game ^. #name) <> " with ID " <> Text.pack (show romId)
  for_ (game ^. #roms) (persistFile romId)

persistFile ::
  [Reader Config, Sql, IOE] :>> es =>
  Persistent.RomId ->
  Dat.Rom ->
  Eff es ()
persistFile romId Dat.Rom { .. } = do
  fileId <- sql $ insert Persistent.File
    { lastCheck = Nothing
    , ..
    }
  sql $ insert_ $ Persistent.RomFile romId fileId
  logText $ "added " <> name <> " with ID " <> Text.pack (show fileId)

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
