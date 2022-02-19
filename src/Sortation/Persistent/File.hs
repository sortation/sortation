module Sortation.Persistent.File where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time.Clock
import Data.Word
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics
import Sortation.Persistent.Quasi

data FileStatus = FileStatus
  { checkTime :: UTCTime
  , existsCheck :: Bool
  , sizeCheck :: Maybe Bool
  , crcCheck :: Maybe Bool
  , md5Check :: Maybe Bool
  , sha1Check :: Maybe Bool
  } deriving (Generic, Eq, Ord, Show, Read)

derivePersistField "FileStatus"

nonexistentFileStatus :: UTCTime -> FileStatus
nonexistentFileStatus checkTime = FileStatus
  { checkTime
  , existsCheck = False
  , sizeCheck = Just False
  , crcCheck = Just False
  , md5Check = Just False
  , sha1Check = Just False
  }

persist "File" [persistLowerCase|
  File
    name Text
    size Word
    crc (Maybe Word32)
    md5 (Maybe ByteString)
    sha1 (Maybe ByteString)
    lastCheck (Maybe FileStatus)
|]
