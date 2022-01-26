module Sortation.Config where

import Control.Monad.IO.Class
import GHC.Generics
import GHC.Natural
import Options.Applicative
import System.IO qualified as System
import System.Path qualified as Path
import System.Path.PartClass qualified as Path.PartClass

parsePath ::
  forall fd m.
  (MonadIO m, Path.PartClass.FileDir fd) =>
  System.FilePath ->
  m (Path.Abs fd)
parsePath = liftIO . Path.dynamicMakeAbsoluteFromCwd . Path.absRel @fd

data GlobalConfig = GlobalConfig
  { bufferSize :: Natural
  , threadCount :: Natural
  , gameDirectory :: System.FilePath
  , datFile :: System.FilePath
  } deriving (Generic, Eq, Ord, Show)

data HashConfig = HashConfig
  { crc :: Bool
  , sha1 :: Bool
  , md5 :: Bool
  } deriving (Generic, Eq, Ord, Show)

data CommandConfig
  = Check CheckConfig

data CheckConfig = CheckConfig
  { hashConfig :: HashConfig
  , flatten :: FlattenOption
  , globalConfig :: GlobalConfig
  } deriving (Generic, Eq, Ord, Show)

data FlattenOption
  = FlattenAlways
  | FlattenSingle
  | FlattenNever
  deriving (Generic, Eq, Ord, Show)

optionsParser :: ParserInfo CommandConfig
optionsParser =
  info (configParser <**> helper) $ mconcat
    [ fullDesc
    , header "sortation"
    ]

configParser :: Parser CommandConfig
configParser = do
  bufferSize <-
    option auto $ mconcat
      [ long "buffer-size", short 'b'
      , help "size of output buffer when processing games"
      , showDefault
      , metavar "NUM"
      , value 1
      ]
  threadCount <-
    option auto $ mconcat
      [ long "thread-count", short 'n'
      , help "number of threads to use when processing games"
      , showDefault
      , metavar "NUM"
      , value 1
      ]
  gameDirectory <-
    strOption $ mconcat
      [ long "rom-directory", short 'd'
      , help "absolute base directory where games/roms are located"
      , showDefault
      , metavar "DIR"
      , action "directory"
      , value "."
      ]
  crc <-
    flag True False $ mconcat
      [ long "disable-crc", short 'c'
      , help "disable crc hash checking"
      , showDefault
      ]
  sha1 <-
    flag True False $ mconcat
      [ long "disable-sha1", short 's'
      , help "disable sha1 hash checking"
      , showDefault
      ]
  md5 <-
    flag True False $ mconcat
      [ long "disable-md5", short 'm'
      , help "disable md5 hash checking"
      , showDefault
      ]
  flatten <-
    option readFlattenOption $ mconcat
      [ long "flatten", short 'f'
      , help "choose when a game gets a folder for its roms: always, single, never"
      , showDefaultWith showFlattenOption
      , completeWith ["always", "single", "never"]
      , value FlattenSingle
      ]
  datFile <-
    argument str $ mconcat
      [ help "dat file to process"
      , metavar "DAT"
      , action "file"
      ]
  pure $ Check CheckConfig
    { globalConfig = GlobalConfig { .. }
    , hashConfig = HashConfig { .. }
    , ..
    }

readFlattenOption :: ReadM FlattenOption
readFlattenOption =
  maybeReader \case
    "always" -> Just FlattenAlways
    "single" -> Just FlattenSingle
    "never" -> Just FlattenNever
    _ -> Nothing

showFlattenOption :: FlattenOption -> String
showFlattenOption = \case
  FlattenAlways -> "always"
  FlattenSingle -> "single"
  FlattenNever -> "never"
