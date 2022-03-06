module Sortation.Library where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Time.Clock
import Data.LanguageCodes
import Data.Serialize
import Data.Serialize.Text ()
import Data.Vector.Instances ()
import Data.Vector.Serialize ()
import Effectful.Database

data Library :: forall k. k -> Type

type instance BackendFileName (Library db) = AppendSymbol "library-" (BackendFileName db)


instance HasBackend (Library db) where
  -- type LibraryBackend :: forall k. k -> Type
  data Backend (Library db) = LibraryBackend
    { romSets :: IntMap (RomSet db)
    , roms :: IntMap (Rom db)
    , files :: IntMap File
    }
    deriving stock (Generic, Eq, Ord, Show, Read)
    deriving anyclass Serialize

  type BackendOpticKind (Library db) = A_Lens
  newBackend = LibraryBackend
    { romSets = IntMap.empty
    , roms = IntMap.empty
    , files = IntMap.empty
    }

instance HasTable (Library db) (RomSet db) where
  tableOptic = #romSets

instance HasTable (Library db) (Rom db) where
  tableOptic = #roms

instance HasTable (Library db) File where
  tableOptic = #files

data Group
  = NoIntro
  deriving stock (Generic, Eq, Ord, Show, Read)
  deriving anyclass Serialize

data RomSet db = RomSet
  { name :: Text
  , directory :: FilePath
  , description :: Maybe Text
  , datFile :: Maybe FilePath
  , category :: Maybe Text
  , version :: Maybe Text
  , date :: Maybe Text
  , author :: Maybe Text
  , email :: Maybe Text
  , group :: Maybe Group
  , url :: Maybe Text
  , comment :: Maybe Text
  , roms :: Vector (Key (Library db) (Rom db))
  }
  deriving stock (Generic, Eq, Ord, Show, Read)
  deriving anyclass Serialize

emptyRomSet :: Text -> FilePath -> RomSet db
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
  , roms = []
  }

data Rom db = Rom
  { name :: Text
  , description :: Text
  , identifier :: Maybe Text
  , regions :: Set Text
  , languages :: Set ISO639_1
  , version :: Maybe Text
  , date :: Maybe Text
  , disc :: Maybe Text
  , files :: Vector (Key (Library db) File)
  }
  deriving stock (Generic, Eq, Ord, Show, Read)
  deriving anyclass Serialize

data FileStatus = FileStatus
  { checkTime :: UTCTime
  , existsCheck :: Bool
  , sizeCheck :: Maybe Bool
  , crcCheck :: Maybe Bool
  , md5Check :: Maybe Bool
  , sha1Check :: Maybe Bool
  }
  deriving stock (Generic, Eq, Ord, Show, Read)
  deriving anyclass Serialize

nonexistentFileStatus :: UTCTime -> FileStatus
nonexistentFileStatus checkTime = FileStatus
  { checkTime
  , existsCheck = False
  , sizeCheck = Just False
  , crcCheck = Just False
  , md5Check = Just False
  , sha1Check = Just False
  }

data File = File
  { name :: Text
  , size :: Word
  , crc :: Maybe Word32
  , md5 :: Maybe ByteString
  , sha1 :: Maybe ByteString
  , lastCheck :: Maybe FileStatus
  }
  deriving stock (Generic, Eq, Ord, Show, Read)
  deriving anyclass Serialize

instance Serialize UTCTime where
  put = put . show
  get = do
    text <- get
    maybe (fail ("invalid time " ++ text)) pure $
      readMaybe text

instance Serialize ISO639_1 where
  put = put . toChars
  get = do
    chars <- get
    maybe (fail ("invalid language code " ++ [fst chars, snd chars])) pure $
      uncurry fromChars chars
