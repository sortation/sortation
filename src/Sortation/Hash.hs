module Sortation.Hash where

import Control.Monad
import Control.Monad.IO.Class
import Crypto.Hash.MD5 qualified as MD5
import Crypto.Hash.SHA1 qualified as SHA1
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Conduit
import Data.Conduit.Combinators qualified as Conduit
import Data.Digest.CRC32 as CRC32
import Data.Word
import Generic.Data
import Optics
import Sortation.Config
import System.Path.IO

data HashState = HashState
  { crc :: Maybe Word32
  , sha1 :: Maybe SHA1.Ctx
  , md5 :: Maybe MD5.Ctx
  } deriving (Generic, Eq)

data Hash = Hash
  { crc :: Maybe Word32
  , sha1 :: Maybe ByteString
  , md5 :: Maybe ByteString
  } deriving (Generic, Eq)

hashFile :: MonadIO m => HashConfig -> Handle -> ConduitT i o m Hash
hashFile config h = 
  fmap finalizeHashState $
    Conduit.sourceHandle h .|
      Conduit.foldl updateHashState (initHashState config)

initHashState :: HashConfig -> HashState
initHashState config =
  HashState
    { crc = crc32 ByteString.empty <$ guard (config ^. #crc)
    , sha1 = SHA1.init <$ guard (config ^. #sha1)
    , md5 = MD5.init <$ guard (config ^. #md5)
    }

updateHashState :: HashState -> ByteString -> HashState
updateHashState hash bytes =
  hash
    & #crc % mapped %~ flip crc32Update bytes
    & #sha1 % mapped %~ flip SHA1.update bytes
    & #md5 % mapped %~ flip MD5.update bytes

finalizeHashState :: HashState -> Hash
finalizeHashState hash = Hash
  { crc = hash ^. #crc
  , sha1 = SHA1.finalize <$> (hash ^. #sha1)
  , md5 = MD5.finalize <$> (hash ^. #md5)
  }
