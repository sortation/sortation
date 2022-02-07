module Sortation.Check where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Resource
import Data.ByteString (ByteString)
import Data.Conduit
import Data.Conduit.Combinators qualified as Conduit
import Data.Conduit.ConcurrentMap
import Data.Conduit.Lift
import Data.List qualified as List
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as Text (toStrict)
import Data.Traversable
import Generic.Data
import GHC.Natural
import Optics
import Sortation.Config
import Sortation.Hash
import System.Path ((</>), relFile, relDir)
import System.Path qualified as Path
import System.Path.Part
import System.Path.Directory
import System.Path.IO
import Text.Dat
import Text.Pretty.Simple

-- data DatReport = DatReport
--   { total :: Sum Natural
--   , missing :: Sum Natural
--   , broken :: Sum Natural
--   } deriving (Generic, Eq, Ord, Show)
--     deriving (Semigroup, Monoid) via (Generically DatReport)
-- 
-- data GameReport = GameReport
--   { romReports :: [(Text, Maybe RomReport)]
--   } deriving (Generic, Eq, Ord, Show)
-- 
-- data RomReport = RomReport
--   { size :: Bool
--   , crc :: Maybe Bool
--   , sha1 :: Maybe Bool
--   , md5 :: Maybe Bool
--   } deriving (Generic, Eq, Ord, Show)
-- 
-- newlines :: Natural -> Text
-- newlines = Text.unlines . flip List.genericReplicate ""
-- 
-- reportDat ::
--   (MonadReader CheckConfig m, MonadUnliftIO m, MonadResource m) =>
--   Maybe Header ->
--   ConduitT Game ByteString m ()
-- reportDat header = datText .| Conduit.map Text.encodeUtf8
--   where
--     datText = do
--       traverse (yield . prettyHeader) header
--       yield $ newlines 3
--       dat <- checkDat `fuseUpstream` Conduit.map (uncurry prettyGame)
--       yield $ newlines 1
--       yield $ prettyDat dat
-- 
-- prettyHeader :: Header -> Text
-- prettyHeader = Text.toStrict . pShowNoColor
-- 
-- prettyDat :: DatReport -> Text
-- prettyDat dat =
--   Text.unlines
--     [ "Total: " <> Text.pack (show (dat ^. #total ^. to getSum))
--     , "Missing: " <> Text.pack (show (dat ^. #missing ^. to getSum))
--     , "Broken: " <> Text.pack (show (dat ^. #broken ^. to getSum))
--     ]
-- 
-- prettyGame :: Text -> GameReport -> Text
-- prettyGame name game =
--   Text.unlines $
--     name : map (("  " <>) . uncurry prettyRom) (game ^. #romReports)
-- 
-- prettyRom :: Text -> Maybe RomReport -> Text
-- prettyRom name rom =
--   Text.unlines
--     [ name
--     , Text.append "    " $ mconcat $
--         List.intersperse " | " $ fmap mconcat $ filter (not . null)
--           [ [ "missing" | rom == Nothing ]
--           , [ "bad size" | rom ^. mapping #size == Just False ]
--           , [ "bad crc" | rom ^. mapping #crc == Just (Just False) ]
--           , [ "bad sha1" | rom ^. mapping #sha1 == Just (Just False) ]
--           , [ "bad md5" | rom ^. mapping #md5 == Just (Just False) ]
--           ]
--     ]

-- checkDat ::
--   (MonadReader CheckConfig m, MonadUnliftIO m, MonadResource m) =>
--   ConduitT Game (Text, GameReport) m DatReport
-- checkDat = do
--   threadCount <- gviews (#globalConfig % #threadCount) fromIntegral
--   bufferSize <- gviews (#globalConfig % #bufferSize) fromIntegral
--   concurrentMapM_ threadCount bufferSize checkGame .|
--     execStateC mempty (Conduit.iterM (modify . mappend . gameDat . snd))
-- 
-- gameDat :: GameReport -> DatReport
-- gameDat game =
--   let roms = game ^. #romReports % mapping _2 in
--     DatReport
--       { total = 1
--       , missing = if any (== Nothing) roms then 1 else 0
--       , broken = if all goodRom roms then 0 else 1
--       }
-- 
-- goodRom :: Maybe RomReport -> Bool
-- goodRom =
--   fromMaybe False . fmap \rom -> and @[]
--     [ rom ^. #size
--     , fromMaybe True (rom ^. #crc)
--     , fromMaybe True (rom ^. #sha1)
--     , fromMaybe True (rom ^. #md5)
--     ]
-- 
-- checkGame ::
--   (MonadIO m, MonadReader CheckConfig m) =>
--   Game -> m (Text, GameReport)
-- checkGame game = do
--   romReports <- traverse checkRom (game ^. #roms)
--   pure (game ^. #name, GameReport { .. })
-- 
-- resolveRomPaths ::
--   (MonadIO m, MonadReader CheckConfig m) =>
--   FlattenOption ->
--   Game ->
--   m [(Path.AbsFile, Rom)]
-- resolveRomPaths opt game =
--   for (game ^. #roms) \rom -> do
--     gameDir <- parsePath @Dir =<< gview (#globalConfig % #gameDirectory)
--     let romDir = Path.makeAbsolute gameDir $ relDir (Text.unpack (game ^. #name))
--     pure $ (,rom) $ relFile (Text.unpack (game ^. #name)) & Path.makeAbsolute
--       case opt of
--         FlattenAlways -> gameDir
--         FlattenSingle | length (game ^. #roms) == 1 -> gameDir
--         FlattenSingle -> romDir
--         FlattenNever -> romDir
-- 
-- checkRom ::
--   (MonadIO m, MonadReader CheckConfig m) =>
--   Rom -> m (Text, Maybe RomReport)
-- checkRom rom = do
--   gameDirectory <- parsePath @Dir =<< gview (#globalConfig % #gameDirectory)
--   let romFile = gameDirectory </> relFile (Text.unpack (rom ^. #name))
--   liftIO (doesFileExist romFile) >>= fmap (rom ^. #name,) . \case
--     False -> pure Nothing
--     True -> do
--       h <- liftIO $ openFile romFile ReadMode
--       fileSize <- liftIO $ hFileSize h
--       let size = toInteger (rom ^. #size) == fileSize
--       hashConfig <- gview #hashConfig
--       hashes <- runConduit (hashFile hashConfig h)
--       liftIO $ hClose h
--       let crc = liftA2 (==) (rom ^. #crc) (hashes ^. #crc)
--       let sha1 = liftA2 (==) (rom ^. #sha1) (hashes ^. #sha1)
--       let md5 = liftA2 (==) (rom ^. #md5) (hashes ^. #md5)
--       pure $ Just RomReport { .. }
