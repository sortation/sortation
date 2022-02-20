module Sortation.Library where

import Data.ByteString (ByteString)
import Data.CasMap (CasKey, CasMap)
import Data.CasMap qualified as CasMap
import Data.Kind
import Data.Time.Clock
import Data.LanguageCodes
import Data.Hashable
import Data.Maybe
import Data.Serialize
import Data.Serialize.Text ()
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.Instances ()
import Data.Vector.Serialize ()
import Data.Word
import Effectful.Database
import Optics
import GHC.Generics
import GHC.TypeLits
import Text.Read (readMaybe)

data Library :: forall k. k -> Type

type instance BackendFileName (Library db) = AppendSymbol "library-" (BackendFileName db)

type LibraryBackend :: forall k. k -> Type
data LibraryBackend db = LibraryBackend
  { romSets :: CasMap (Library db) (RomSet (Key (Library db) (Rom (Key (Library db) File))))
  , roms :: CasMap (Library db) (Rom (Key (Library db) File))
  , files :: CasMap (Library db) File
  }
  deriving stock (Generic, Eq, Ord, Show, Read)
  deriving anyclass Serialize

instance HasBackend (Library db) where
  type Backend (Library db) = LibraryBackend db
  type TableOpticKind (Library db) = A_Lens
  newBackend = LibraryBackend
    { romSets = CasMap.empty
    , roms = CasMap.empty
    , files = CasMap.empty
    }

instance HasTable (Library db) (RomSet (Key (Library db) (Rom (Key (Library db) File)))) where
  tableOptic = #romSets

instance HasTable (Library db) (Rom (Key (Library db) File)) where
  tableOptic = #roms

instance HasTable (Library db) File where
  tableOptic = #files

instance Hashable ISO639_1 where
  hashWithSalt salt = hashWithSalt salt . toChars

data Group
  = NoIntro
  deriving stock (Generic, Eq, Ord, Show, Read)
  deriving anyclass (Hashable, Serialize)

data RomSet rom = RomSet
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
  , roms :: Vector rom
  }
  deriving stock (Generic, Eq, Ord, Show, Read, Functor, Foldable, Traversable)
  deriving anyclass (Hashable, Serialize)

emptyRomSet :: Text -> FilePath -> RomSet rom
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

data Rom file = Rom
  { name :: Text
  , description :: Text
  , identifier :: Maybe Text
  , regions :: Set Text
  , languages :: Set ISO639_1
  , version :: Maybe Text
  , date :: Maybe Text
  , disc :: Maybe Text
  , files :: Vector file
  }
  deriving stock (Generic, Eq, Ord, Show, Read, Functor, Foldable, Traversable)
  deriving anyclass (Hashable, Serialize)

data FileStatus = FileStatus
  { checkTime :: UTCTime
  , existsCheck :: Bool
  , sizeCheck :: Maybe Bool
  , crcCheck :: Maybe Bool
  , md5Check :: Maybe Bool
  , sha1Check :: Maybe Bool
  }
  deriving stock (Generic, Eq, Ord, Show, Read)
  deriving anyclass (Hashable, Serialize)

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
  deriving anyclass (Hashable, Serialize)

instance Hashable UTCTime where
  hashWithSalt salt = hashWithSalt salt . show

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
