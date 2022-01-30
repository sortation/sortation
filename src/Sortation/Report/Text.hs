module Sortation.Report.Text where

import Control.Monad.Reader
import Data.Conduit
import Data.Conduit.Combinators qualified as Conduit
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Database.Persist.Pagination
import Database.Persist.Sql
import Optics
import Sortation.Persistent

stream ::
  ( MonadIO m
  , PersistEntity a
  , PersistQueryRead b
  , PersistEntityBackend a ~ BaseBackend b
  ) =>
  EntityField a (Key a) ->
  [Filter a] ->
  ConduitT i (Entity a) (ReaderT b m) ()
stream field filters =
  streamEntities
    filters
    field
    (PageSize 1)
    Ascend
    (Range Nothing Nothing)

gameFilter :: CollectionId -> Filter Game
gameFilter collectionId = Filter GameParents (FilterValue (Set.singleton collectionId)) In

releaseFilter :: GameId -> Filter Release
releaseFilter gameId = Filter ReleaseParent (FilterValue gameId) Eq

romFilter :: ReleaseId -> Filter Rom
romFilter releaseId = Filter RomParent (FilterValue releaseId) Eq

printCollections ::
  MonadIO m =>
  ReaderT SqlBackend m ()
printCollections =
  runConduit $ stream CollectionId [] .| Conduit.mapM_ \collection -> do
    liftIO $ Text.putStrLn $ "COLLECTION: " <> collection ^. #entityVal % #name
    runConduit $ stream GameId [gameFilter (collection ^. #entityKey)] .| Conduit.mapM_ \game -> do
      liftIO $ Text.putStrLn $ "  GAME: " <> game ^. #entityVal % #name
      runConduit $ stream ReleaseId [releaseFilter (game ^. #entityKey)] .| Conduit.mapM_ \release -> do
        liftIO $ Text.putStrLn $ "    RELEASE: " <> releaseString (release ^. #entityVal)
        runConduit $ stream RomId [romFilter (release ^. #entityKey)] .| Conduit.mapM_ \rom -> do
          liftIO $ Text.putStrLn $ "      ROM: " <> rom ^. #entityVal % #name
          liftIO $ Text.putStrLn $ "        PATH: " <> rom ^. #entityVal % #path
          liftIO $ Text.putStrLn $ "        SIZE: " <> Text.pack (show (rom ^. #entityVal % #size))
          liftIO $ Text.putStrLn $ "        CRC: " <> Text.pack (show (rom ^. #entityVal % #crc))
          liftIO $ Text.putStrLn $ "        MD5: " <> Text.pack (show (rom ^. #entityVal % #md5))
          liftIO $ Text.putStrLn $ "        SHA1: " <> Text.pack (show (rom ^. #entityVal % #sha1))
