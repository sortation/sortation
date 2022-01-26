module Sortation.Process where

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Conduit.ConcurrentMap
import Optics
import Sortation.Config
import Text.Dat

processDat ::
  ( MonadReader c m, MonadUnliftIO m, MonadResource m
  , HasGlobalConfig c
  ) =>
  (Game -> m o) ->
  ConduitT Game o m ()
processDat processGame = do
  threadCount <- gviews (globalConfigL % #threadCount) fromIntegral
  bufferSize <- gviews (globalConfigL % #bufferSize) fromIntegral
  concurrentMapM_ threadCount bufferSize processGame
