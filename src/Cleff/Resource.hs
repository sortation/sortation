module Cleff.Resource where

import Cleff
import Control.Monad.Trans.Resource

instance IOE :> es => MonadResource (Eff es) where
  liftResourceT = liftIO . runResourceT
