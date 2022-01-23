module Text.Dat where

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.ByteString (ByteString)
import Data.Conduit
import Data.Kind
import Data.LargeWord
import Data.Text (Text)
import Data.Word
import GHC.Generics
import GHC.Natural
import Optics

type HeaderT :: (Type -> Type) -> Type -> Type
newtype HeaderT m a = HeaderT { runHeaderT :: Maybe Header -> m a }
  deriving
    ( Functor, Applicative, Monad
    , MonadThrow, MonadIO, MonadUnliftIO, MonadResource
    ) via ReaderT (Maybe Header) m
  deriving
    ( MonadTrans
    ) via ReaderT (Maybe Header)

instance MonadReader r m => MonadReader r (HeaderT m) where
  ask = lift ask
  local f = HeaderT . fmap (local f) . runHeaderT

askHeader :: Applicative m => HeaderT m (Maybe Header)
askHeader = HeaderT pure

runHeaderC :: Monad m => Maybe Header -> ConduitT i o (HeaderT m) r -> ConduitT i o m r
runHeaderC h = transPipe (($ h) . runHeaderT)

data Header = Header
  { name :: Text
  , description :: Text
  , category :: Maybe Text
  , version :: Text
  , date :: Maybe Text
  , author :: Text
  , email :: Maybe Text
  , homepage :: Maybe Text
  , url :: Maybe Text
  , comment :: Maybe Text
  , clrMamePro :: Maybe ClrMameProHeader
  , romCenter :: Maybe RomCenterHeader
  } deriving (Generic, Show, Eq, Ord)

data ClrMameProHeader = ClrMameProHeader
  { header :: Maybe Text
  , forceMerging :: MergeType
  , forceNoDump :: NoDumpType
  , forcePacking :: PackingType
  } deriving (Generic, Show, Eq, Ord)

data MergeType
  = NoMerge
  | SplitMerge
  | FullMerge
  deriving (Generic, Show, Eq, Ord)

data NoDumpType
  = Obsolete
  | Required
  | Ignore
  deriving (Generic, Show, Eq, Ord)

data PackingType
  = Zip
  | Unzip
  deriving (Generic, Show, Eq, Ord)

data RomCenterHeader = RomCenterHeader
  { plugin :: Maybe Text
  , romMode :: RomSetType
  , biosMode :: RomSetType
  , sampleMode :: SampleSetType
  , lockRomMode
  , lockBiosMode
  , lockSampleMode :: Bool
  }
  deriving (Generic, Show, Eq, Ord)

data RomSetType
  = Merged
  | Split
  | Unmerged
  deriving (Generic, Show, Eq, Ord)

data SampleSetType
  = MergedSamples
  | UnmergedSamples
  deriving (Generic, Show, Eq, Ord)

data Game = Game
  { name :: Text
  , sourceFile :: Maybe Text
  , isBios :: Bool
  , cloneOf :: Maybe Text
  , romOf :: Maybe Text
  , sampleOf :: Maybe Text
  , board :: Maybe Text
  , rebuildTo :: Maybe Text
  , comments :: [Text]
  , description :: Text
  , year :: Maybe Text
  , manufacturer :: Maybe Text
  , releases :: [Release]
  , biosSets :: [BiosSet]
  , roms :: [Rom]
  , disks :: [Disk]
  , samples :: [Sample]
  , archives :: [Archive]
  } deriving (Generic, Show, Eq, Ord)

data Release = Release
  { name :: Text
  , region :: Text
  , language :: Maybe Text
  , date :: Maybe Text
  , _default :: Bool
  } deriving (Generic, Show, Eq, Ord)

data BiosSet = BiosSet
  { name :: Text
  , description :: Text
  , _default :: Bool
  } deriving (Generic, Show, Eq, Ord)

data Rom = Rom
  { name :: Text
  , size :: Natural
  , crc :: Maybe Word32
  , sha1 :: Maybe ByteString
  , md5 :: Maybe ByteString
  , merge :: Maybe Text
  , status :: RomStatus
  , date :: Maybe Text
  } deriving (Generic, Show, Eq, Ord)

data Disk = Disk
  { name :: Text
  , sha1 :: Maybe Word160
  , md5 :: Maybe Word128
  , merge :: Maybe Text
  , status :: RomStatus
  } deriving (Generic, Show, Eq, Ord)

newtype Sample = Sample
  { name :: Text
  } deriving (Generic, Show, Eq, Ord)

newtype Archive = Archive
  { name :: Text
  } deriving (Generic, Show, Eq, Ord)

data RomStatus
  = BadDump
  | NoDump
  | Good
  | Verified
  deriving (Generic, Show, Eq, Ord)

makePrismLabels ''MergeType
makePrismLabels ''NoDumpType
makePrismLabels ''PackingType
makePrismLabels ''RomSetType
makePrismLabels ''SampleSetType
makePrismLabels ''RomStatus
