module Sortation.Ingest.Config where

import Data.Text (Text)
import GHC.Generics
import Optics
import Optics.TH
import Options.Applicative

data Config = Config
  { name :: Text
  , datFile :: Text
  , rootDir :: Text
  , namingConventionOption :: NamingConventionOption
  , hierarchy :: Hierarchy
  , verbose :: Bool
  } deriving (Generic, Eq, Ord, Show)

data NamingConvention
  = NoIntro
  deriving (Generic, Eq, Ord, Show)

type NamingConventionOption = Maybe (Maybe NamingConvention)

pattern Detect :: NamingConventionOption
pattern Detect = Just Nothing

fromNamingConventionOption ::
  Maybe NamingConvention ->
  NamingConventionOption ->
  Maybe NamingConvention
fromNamingConventionOption def = \case
  Nothing -> Nothing
  Just Nothing -> def
  Just (Just namingConvention) -> Just namingConvention

data Hierarchy
  = Flat
  | Mixed
  | Nested
  deriving (Generic, Eq, Ord, Show)

makePrismLabels ''NamingConvention
makePrismLabels ''Hierarchy

configParserInfo :: ParserInfo Config
configParserInfo =
  info (configParser <**> helper) $ mconcat
    [ fullDesc
    , header "ingest"
    ]

configParser :: Parser Config
configParser = do
  name <-
    strOption $ mconcat
      [ long "name", short 'n'
      , help "collection name"
      , metavar "NAME"
      ]
  rootDir <-
    strOption $ mconcat
      [ long "rom-directory", short 'd'
      , help "base directory where games/roms are located"
      , showDefault
      , metavar "DIR"
      , action "directory"
      , value "."
      ]
  namingConventionOption <-
    option readNamingConvention $ mconcat
      [ long "naming-convention", short 'n'
      , help "rom naming convention: no-intro, detect, none"
      , showDefaultWith showNamingConvention
      , completeWith ["no-intro", "detect", "none"]
      , value $ Just Nothing
      ]
  hierarchy <-
    option readHierarchy $ mconcat
      [ long "hierarchy", short 'r'
      , help "folder hierarchy: flat, mixed, nested"
      , showDefaultWith showHierarchy
      , completeWith ["flat", "mixed", "nested"]
      , value Mixed
      ]
  verbose <-
    switch $ mconcat
      [ long "verbose", short 'v'
      , help "log to stdout"
      ]
  datFile <-
    strArgument $ mconcat
      [ help "dat file to ingest"
      , action "file"
      , metavar "DAT"
      ]
  pure Config { .. }

readNamingConvention :: ReadM NamingConventionOption
readNamingConvention =
  maybeReader \case
    "no-intro" -> Just $ Just $ Just NoIntro
    "detect" -> Just $ Just Nothing
    "none" -> Just Nothing
    _ -> Nothing

showNamingConvention :: NamingConventionOption -> String
showNamingConvention = \case
  Just (Just NoIntro) -> "no-intro"
  Just Nothing -> "detect"
  Nothing -> "none"

readHierarchy :: ReadM Hierarchy
readHierarchy =
  maybeReader \case
    "flat" -> Just Flat
    "mixed" -> Just Mixed
    "nested" -> Just Nested
    _ -> Nothing

showHierarchy :: Hierarchy -> String
showHierarchy = \case
  Flat -> "flat"
  Mixed -> "mixed"
  Nested -> "nested"
