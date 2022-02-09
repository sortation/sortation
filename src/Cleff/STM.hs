module Cleff.STM where

import Cleff
import Control.Concurrent.STM as STM

atomically :: IOE :> es => STM a -> Eff es a
atomically = liftIO . STM.atomically
