module Data.Conduit.Concurrent where

import Cleff
import Cleff.Optics
import Cleff.Reader
import Cleff.STM
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Concurrent.STM.TBMChan
import Control.Monad.Loops
import Data.Conduit
import Data.Conduit.Combinators qualified as Conduit
import Data.Conduit.TMChan
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Maybe
import Data.Traversable
import GHC.Generics
import System.Random

data UnorderedMapConfig = UnorderedMapConfig
  { bufferSize :: Int
  , threadCount :: Int
  } deriving (Generic, Eq, Ord, Show)

unorderedMapM_ ::
  [Reader UnorderedMapConfig, IOE] :>> es =>
  (i -> Eff es ()) ->
  ConduitT i o (Eff es) ()
unorderedMapM_ task = do
  bufferSize <- lift $ peruse @UnorderedMapConfig #bufferSize
  threadCount <- lift $ peruse @UnorderedMapConfig #threadCount
  chan <- liftIO $ newTBMChanIO bufferSize
  signals <- liftIO $ replicateM threadCount newEmptyMVar
  for_ signals \signal -> lift $ withRunInIO \unlift ->
    forkIO $ unlift do
      whileJust_ (atomically (readTBMChan chan)) task
      liftIO $ putMVar signal ()
  sinkTBMChan chan
  lift $ atomically $ closeTBMChan chan
  for_ signals (liftIO . void . takeMVar)

tryReadTBMChans :: IOE :> es => [TBMChan a] -> Eff es (Maybe (Maybe a))
tryReadTBMChans [] = pure Nothing
tryReadTBMChans (chan : chans) =
  atomically (tryReadTBMChan chan) >>= \case
    Nothing -> tryReadTBMChans chans
    Just Nothing -> tryReadTBMChans chans <&> (<|> Just Nothing)
    Just (Just x) -> pure $ Just $ Just x

unorderedMapM ::
  [Reader UnorderedMapConfig, IOE] :>> es =>
  (i -> Eff es o) ->
  ConduitT i o (Eff es) ()
unorderedMapM f = do
  bufferSize <- lift $ peruse @UnorderedMapConfig #bufferSize
  threadCount <- lift $ peruse @UnorderedMapConfig #threadCount
  inChan <- liftIO $ newTBMChanIO bufferSize
  outChans <- liftIO $ replicateM threadCount $ newTBMChanIO bufferSize
  for_ outChans \outChan -> lift $ withRunInIO \unlift ->
    forkIO $ unlift do
      whileJust_ (atomically (readTBMChan inChan))
        (atomically . writeTBMChan outChan <=< f)
      atomically $ closeTBMChan outChan
  let
    flushOnce andThen =
      lift (tryReadTBMChans outChans) >>= \case
        Nothing -> pure ()
        Just Nothing -> andThen
        Just (Just o) -> yield o *> andThen
  fix \inStep ->
    await >>= \case
      Nothing -> pure ()
      Just i ->
        fix \writeStep ->
          lift (atomically (tryWriteTBMChan inChan i)) >>= \case
            Just True -> inStep
            Nothing -> pure ()
            Just False -> flushOnce writeStep
  lift $ atomically $ closeTBMChan inChan
  fix flushOnce
