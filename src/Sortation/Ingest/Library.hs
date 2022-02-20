module Sortation.Ingest.Library where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans
import Data.Attoparsec.Text
import Data.IORef
import Data.Conduit
import Data.Conduit.Combinators qualified as Conduit
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, pack, unpack)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Effectful
import Effectful.Database
import Effectful.Internal.Monad
import Effectful.Reader.Static
import GHC.Generics
import Optics
import Sortation.Library
import Sortation.Ingest.Config (Config(..))
import Sortation.Ingest.Config qualified as Config
import Sortation.Format.Dat qualified as Dat
import Sortation.Format.Dat.Parse
import Sortation.Format.NoIntro qualified as NoIntro
import System.IO
import Text.XML.Stream.Parse qualified as XML

ingestLibrary ::
  forall db es.
  ( Is (TableOpticKind db) A_Getter
  , Is (TableOpticKind db) A_Setter
  , HasTable db (RomSet (Key db (Rom (Key db File))))
  , HasTable db (Rom (Key db File))
  , HasTable db File
  , [Reader Config, Database db, IOE] :>> es
  ) =>
  Eff es ()
ingestLibrary = do
  headerRef <- liftIO $ newIORef Nothing
  bracket
    do liftIO . flip openFile ReadMode . unpack . view #datFile =<< ask @Config
    do liftIO . hClose
    do
      \handle -> runConduit $
        Conduit.sourceHandle handle
          .| XML.parseBytes XML.def
          .| parseDat (liftIO . writeIORef headerRef)
          .| (ingestGames @db =<< liftIO (readIORef headerRef))

logText :: [Reader Config, IOE] :>> es => Text -> Eff es ()
logText msg = do
  verbose <- view #verbose <$> ask @Config
  when verbose $ liftIO $ Text.putStrLn msg

parseGroup :: Text -> Maybe Group
parseGroup "No-Intro" = Just NoIntro
parseGroup _ = Nothing

groupNamingConvention :: Group -> Config.NamingConvention
groupNamingConvention NoIntro = Config.NoIntro

ingestGames ::
  forall db es o.
  ( Is (TableOpticKind db) A_Getter
  , Is (TableOpticKind db) A_Setter
  , HasTable db (RomSet (Key db (Rom (Key db File))))
  , HasTable db (Rom (Key db File))
  , HasTable db File
  , [Reader Config, Database db, IOE] :>> es
  ) =>
  Maybe Dat.Header ->
  ConduitT Dat.Game o (Eff es) ()
ingestGames header = do
  Config { .. } <- lift ask
  let
    (romSet, namingConvention) = case header of
      Nothing ->
        ( emptyRomSet name (unpack rootDir) & #datFile .~ Just (unpack datFile)
        , Config.fromNamingConventionOption Nothing namingConventionOption
        )
      Just Dat.Header { .. } ->
        let group = parseGroup =<< homepage in
          ( RomSet
            { directory = unpack rootDir
            , description = Just description
            , version = Just version
            , author = Just author
            , datFile = Just $ unpack datFile
            , roms = []
            , ..
            }
          , fmap groupNamingConvention group
          )
  roms <- Conduit.mapM (ingestRom @db namingConvention) .| transPipe runPrim Conduit.sinkVector
  romSetId <- lift $ insert @db (romSet & #roms .~ roms)
  lift $ logText $ "added " <> name <> " with ID " <> Text.pack (show romSetId)

newtype TitleParsingError = TitleParsingError String
  deriving (Generic, Eq, Ord, Show, Read)
  deriving anyclass Exception

parseDatGame ::
  Maybe Config.NamingConvention ->
  Dat.Game ->
  Eff es (Rom (Key db File))
parseDatGame namingConvention Dat.Game { .. } = do
  case namingConvention of
    Nothing -> pure Rom
      { regions = Set.empty
      , languages = Set.empty
      , version = Nothing
      , date = Nothing
      , disc = Nothing
      , files = []
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
        , files = []
        , ..
        }

ingestRom ::
  forall db es.
  ( Is (TableOpticKind db) A_Getter
  , Is (TableOpticKind db) A_Setter
  , HasTable db (Rom (Key db File))
  , HasTable db File
  , [Reader Config, Database db, IOE] :>> es
  ) =>
  Maybe Config.NamingConvention ->
  Dat.Game ->
  Eff es (Key db (Rom (Key db File)))
ingestRom namingConvention game = do
  files <- Vector.fromList <$> traverse (ingestFile @db) (game ^. #roms)
  romId <- insert @db . set #files files =<< parseDatGame namingConvention game
  logText $ "added " <> (game ^. #name) <> " with ID " <> Text.pack (show romId)
  pure romId

ingestFile ::
  forall db es.
  ( Is (TableOpticKind db) A_Getter
  , Is (TableOpticKind db) A_Setter
  , HasTable db File
  , [Reader Config, Database db, IOE] :>> es
  ) =>
  Dat.Rom ->
  Eff es (Key db File)
ingestFile Dat.Rom { .. } = do
  fileId <- insert @db File
    { lastCheck = Nothing
    , ..
    }
  -- sql $ insert_ $ Persistent.RomFile romId fileId
  logText $ "added " <> name <> " with ID " <> Text.pack (show fileId)
  pure fileId
