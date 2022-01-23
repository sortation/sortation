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
  (MonadReader Config m, MonadUnliftIO m, MonadResource m) =>
  (Game -> m o) ->
  ConduitT Game o m ()
processDat processGame = do
  threadCount <- gviews #threadCount fromIntegral
  bufferSize <- gviews #bufferSize fromIntegral
  concurrentMapM_ threadCount bufferSize processGame
