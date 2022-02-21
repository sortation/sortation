module Sortation.Ingest.Dat where

import Data.Attoparsec.Text
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Effectful.Database
import Sortation.Library
import Sortation.Ingest.Config (Config(..))
import Sortation.Ingest.Config qualified as Config
import Sortation.Format.Dat (Dat(..))
import Sortation.Format.Dat qualified as Dat
import Sortation.Format.Dat.Parse
import Sortation.Format.NoIntro qualified as NoIntro

ingest ::
  forall db es.
  ( ModifiableDatabase (Library db)
  , [Reader Config, Database (Library db), IOE] :>> es
  ) =>
  Eff es ()
ingest = ingestDat @db =<< readDat =<< asks @Config (.datFile)

logText :: [Reader Config, IOE] :>> es => Text -> Eff es ()
logText msg = do
  verbose <- asks @Config (.verbose)
  when verbose $ liftIO $ Text.putStrLn msg

parseGroup :: Text -> Maybe Group
parseGroup "No-Intro" = Just NoIntro
parseGroup _ = Nothing

groupNamingConvention :: Group -> Config.NamingConvention
groupNamingConvention NoIntro = Config.NoIntro

ingestDat ::
  forall db es.
  ( ModifiableDatabase (Library db)
  , [Reader Config, Database (Library db), IOE] :>> es
  ) =>
  Dat -> Eff es ()
ingestDat Dat { header, games } = do
  Config { .. } <- ask
  let
    (group, namingConvention) = case header of
      Nothing -> (Nothing, Config.fromNamingConventionOption Nothing namingConventionOption)
      Just Dat.Header { homepage } -> 
        let group = parseGroup =<< homepage in
          (group, fmap groupNamingConvention group)
  romKeys <- traverse (ingestRom @db namingConvention) games
  let
    romSet = case header of
      Nothing ->
        (emptyRomSet name rootDir)
          { datFile = Just datFile
          , roms = romKeys
          }
      Just Dat.Header { .. } ->
        RomSet
          { directory = rootDir
          , description = Just description
          , version = Just version
          , author = Just author
          , datFile = Just datFile
          , roms = romKeys
          , ..
          }
  romSetKey <- insert @(Library db) romSet
  logText $ "added " <> name <> " with key " <> Text.pack (show romSetKey)

newtype TitleParsingError = TitleParsingError String
  deriving (Generic, Eq, Ord, Show, Read)
  deriving anyclass Exception

parseDatGame ::
  Maybe Config.NamingConvention ->
  Vector (Key (Library db) File) ->
  Dat.Game ->
  Eff es (Rom db)
parseDatGame namingConvention files Dat.Game { .. } = do
  case namingConvention of
    Nothing -> pure Rom
      { regions = Set.empty
      , languages = Set.empty
      , version = Nothing
      , date = Nothing
      , disc = Nothing
      , files = files
      , ..
      }
    Just Config.NoIntro -> do
      NoIntro.Title { .. } <-
        either
          do throwM . TitleParsingError
          do pure
          do parseOnly NoIntro.parseTitle name
      pure Rom
        { date = Nothing
        , version = pack . show <$> version
        , files = files
        , ..
        }

ingestRom ::
  forall db es.
  ( ModifiableDatabase (Library db)
  , [Reader Config, Database (Library db), IOE] :>> es
  ) =>
  Maybe Config.NamingConvention ->
  Dat.Game ->
  Eff es (Key (Library db) (Rom db))
ingestRom namingConvention game = do
  fileKeys <- traverse (ingestFile @db) game.roms
  romKey <- insert @(Library db) @(Rom db) =<< parseDatGame namingConvention fileKeys game
  logText $ "added " <> game.name <> " with key " <> Text.pack (show romKey)
  pure romKey

ingestFile ::
  forall db es.
  ( ModifiableDatabase (Library db)
  , [Reader Config, Database (Library db), IOE] :>> es
  ) =>
  Dat.Rom ->
  Eff es (Key (Library db) File)
ingestFile Dat.Rom { .. } = do
  fileId <- insert @(Library db) File
    { lastCheck = Nothing
    , ..
    }
  logText $ "added " <> name <> " with key " <> Text.pack (show fileId)
  pure fileId
