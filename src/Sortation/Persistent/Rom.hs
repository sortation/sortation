module Sortation.Persistent.Rom where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time.Clock
import Data.Word
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics
import Sortation.Persistent.Quasi
import Sortation.Persistent.Release (ReleaseId)

data RomStatus = RomStatus
  { checkTime :: UTCTime
  , existsCheck :: Bool
  , sizeCheck :: Maybe Bool
  , crcCheck :: Maybe Bool
  , md5Check :: Maybe Bool
  , sha1Check :: Maybe Bool
  } deriving (Generic, Eq, Ord, Show, Read)

derivePersistField "RomStatus"

nonexistentRomStatus :: UTCTime -> RomStatus
nonexistentRomStatus checkTime = RomStatus
  { checkTime
  , existsCheck = False
  , sizeCheck = Just False
  , crcCheck = Just False
  , md5Check = Just False
  , sha1Check = Just False
  }

persist "Rom" [persistLowerCase|
  Rom
    parent ReleaseId
    name Text
    path Text
    size Word
    crc (Maybe Word32)
    md5 (Maybe ByteString)
    sha1 (Maybe ByteString)
    lastCheck (Maybe RomStatus)
|]
