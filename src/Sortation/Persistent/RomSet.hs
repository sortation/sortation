module Sortation.Persistent.RomSet where

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
import Sortation.Persistent.Rom (RomId)

data Group
  = NoIntro
  deriving (Generic, Eq, Ord, Show, Read)

derivePersistField "Group"

persist "RomSet" [persistLowerCase|
  RomSet
    name Text
    directory Text
    description (Maybe Text)
    datFile (Maybe Text)
    category (Maybe Text)
    version (Maybe Text)
    date (Maybe Text)
    author (Maybe Text)
    email (Maybe Text)
    group (Maybe Group)
    url (Maybe Text)
    comment (Maybe Text)
|]

persist "RomSetRom" [persistLowerCase|
  RomSetRom
    romSet RomSetId
    rom RomId
    UniqueRomSetRom romSet rom
|]

emptyRomSet :: Text -> Text -> RomSet
emptyRomSet name dir = RomSet
  { name = name
  , directory = dir
  , description = Nothing
  , datFile = Nothing
  , category = Nothing
  , version = Nothing
  , date = Nothing
  , author = Nothing
  , email = Nothing
  , group = Nothing
  , url = Nothing
  , comment = Nothing
  }
