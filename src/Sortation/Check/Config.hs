module Sortation.Check.Config where

import Options.Applicative
import Sortation.Hash

data Config = Config
  { name :: Text
  , bufferSize :: Int
  , threadCount :: Int
  , verbose :: Bool
  , size :: Bool
  , hashConfig :: HashConfig
  } deriving (Generic, Show, Eq, Ord)

configParserInfo :: ParserInfo Config
configParserInfo =
  info (configParser <**> helper) $ mconcat
    [ fullDesc
    , header "check"
    ]

configParser :: Parser Config
configParser = do
  bufferSize <-
    option auto $ mconcat
      [ long "buffer-size", short 'b'
      , help "size of DB streaming buffer"
      , showDefault
      , metavar "NUM"
      , value 1
      ]
  threadCount <-
    option auto $ mconcat
      [ long "thread-count", short 'n'
      , help "number of checking threads"
      , showDefault
      , metavar "NUM"
      , value 1
      ]
  verbose <-
    switch $ mconcat
      [ long "verbose", short 'v'
      , help "log to stdout"
      ]
  size <-
    switch $ mconcat
      [ long "check-size", short 'z'
      , help "check file sizes against database"
      ]
  crc <-
    switch $ mconcat
      [ long "check-crc", short 'c'
      , help "check file CRC32 hashes against database"
      ]
  md5 <-
    switch $ mconcat
      [ long "check-md5", short 'm'
      , help "check file MD5 hashes against database"
      ]
  sha1 <-
    switch $ mconcat
      [ long "check-sha1", short 's'
      , help "check file SHA1 hashes against database"
      ]
  name <-
    strArgument $ mconcat
      [ help "collection to check"
      , metavar "NAME"
      ]
  pure Config { hashConfig = HashConfig { .. }, .. }
