module Sortation.Persistent.Rom where

import Data.ByteString (ByteString)
import Data.LanguageCodes
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time.Clock
import Data.Word
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics
import Sortation.Format.Tags
import Sortation.Persistent.Quasi
import Sortation.Persistent.File (FileId)

persist "Rom" [persistLowerCase|
  Rom
    name Text
    description Text
    identifier (Maybe Text)
    regions (Set Text)
    languages (Set ISO639_1)
    version (Maybe Version)
    date (Maybe Text)
    disc (Maybe Text)
|]

persist "RomFile" [persistLowerCase|
  RomFile
    rom RomId
    file FileId
    UniqueRomFile rom file
|]
