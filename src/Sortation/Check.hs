module Sortation.Check where

import Cleff
import Cleff.Optics
import Cleff.Reader
import Cleff.Sql
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Unlift
import Data.ByteString (ByteString)
import Data.Conduit
import Data.Conduit.Combinators qualified as Conduit
import Data.Conduit.Concurrent
import Data.Conduit.Lift
import Data.Foldable
import Data.Functor
import Data.List qualified as List
import Data.Maybe
import Data.Monoid
import Data.Text (Text, unpack)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as Text (toStrict)
import Data.Text.IO qualified as Text
import Data.Time.Clock
import Data.Traversable
import Database.Persist.Class
import Database.Persist.Sql hiding (Sql)
import Optics
import Sortation.Check.Config
import Sortation.Hash
import Sortation.Persistent
import System.Path (absFile)
import System.Path qualified as Path
import System.Path.Directory
import System.Path.IO

-- checkCollection :: [Reader Config, Sql, IOE] :>> es => Eff es ()
-- checkCollection = do
--   Config { .. } <- ask @Config
--   sql (selectFirst [collectionNameFilter name] []) >>= \case
--     Nothing -> liftIO $ putStrLn "bad collection name"
--     Just collection -> do
--       runReader UnorderedMapConfig { .. } $ runConduit $
--         fuse
--           do
--             stream GameId [gameFilter (collection ^. #entityKey)] .| unorderedMapM \game -> do
--               when verbose $ liftIO $ Text.putStrLn ("GAME " <> game ^. #entityVal % #name)
--               sql (selectList [releaseFilter (game ^. #entityKey)] []) >>= traverse \release -> do
--                 let releaseName = releaseString (release ^. #entityVal)
--                 when verbose $ liftIO $ Text.putStrLn ("  RELEASE " <> releaseName)
--                 sql (selectList [romFilter (release ^. #entityKey)] []) >>= traverse \rom -> do
--                   when verbose $ liftIO $ Text.putStrLn ("    ROM " <> rom ^. #entityVal % #name)
--                   let romFile = absFile (unpack (rom ^. #entityVal % #path))
--                   checkTime <- liftIO getCurrentTime
--                   status <- liftIO (doesFileExist romFile) >>= \case
--                     False -> pure $ nonexistentRomStatus checkTime
--                     True -> withRunInIO \unlift -> do
--                       (fileSize, hashes) <- liftIO $ withFile romFile ReadMode \h -> unlift $
--                         liftA2 (,)
--                           do liftIO if size then Just <$> hFileSize h else pure Nothing
--                           do runConduit $ hashFile hashConfig h
--                       let
--                         sizeCheck = fmap (fromIntegral (rom ^. #entityVal % #size) ==) fileSize
--                         crcCheck = liftA2 (==) (rom ^. #entityVal % #crc) (hashes ^. #crc)
--                         sha1Check = liftA2 (==) (rom ^. #entityVal % #sha1) (hashes ^. #sha1)
--                         md5Check = liftA2 (==) (rom ^. #entityVal % #md5) (hashes ^. #md5)
--                       pure RomStatus { existsCheck = True, .. }
--                   pure (rom ^. #entityKey, [Update RomLastCheck (Just status) Assign])
--           do
--             Conduit.concat .| Conduit.concat .|
--               Conduit.mapM_ (sql . uncurry update)
