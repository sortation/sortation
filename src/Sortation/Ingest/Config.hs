module Sortation.Ingest.Config where

import Data.Text (Text)
import GHC.Generics
import Optics
import Options.Applicative

data Config = Config
  { datFile :: Text
  , rootDir :: Text
  , hierarchy :: Hierarchy
  , verbose :: Bool
  } deriving (Generic, Eq, Ord, Show)

data Hierarchy
  = Flat
  | Mixed
  | Nested
  deriving (Generic, Eq, Ord, Show)

makePrismLabels ''Hierarchy

configParserInfo :: ParserInfo Config
configParserInfo =
  info (configParser <**> helper) $ mconcat
    [ fullDesc
    , header "ingest"
    ]

configParser :: Parser Config
configParser = do
  rootDir <-
    strOption $ mconcat
      [ long "rom-directory", short 'd'
      , help "base directory where games/roms are located"
      , showDefault
      , metavar "DIR"
      , action "directory"
      , value "."
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
