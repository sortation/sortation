module Sortation.Persistent.Rom where

import Database.Persist
import Database.Persist.TH
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Word
import Sortation.Persistent.Quasi
import Sortation.Persistent.Release (ReleaseId)

persist "Rom" [persistLowerCase|
  Rom
    parent ReleaseId
    name Text
    path Text
    size Word
    crc (Maybe Word32)
    md5 (Maybe ByteString)
    sha1 (Maybe ByteString)
|]
