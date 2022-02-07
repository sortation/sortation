module Cleff.Sql where

import Cleff
import Cleff.Reader
import Control.Monad.Logger
import Control.Monad.Reader qualified as MTL
import Data.Text (Text)
import Database.Persist.SqlBackend
import Database.Persist.Sqlite (withSqliteConn)

data Sql :: Effect where
  Sql :: MTL.ReaderT SqlBackend m a -> Sql m a

makeEffect ''Sql

runSqlBackend :: SqlBackend -> Eff (Sql : es) ~> Eff es
runSqlBackend backend = interpret \(Sql m) -> toEff $ MTL.runReaderT m backend

runSilentSqliteConn :: IOE :> es => Text -> Eff (Sql : es) ~> Eff es
runSilentSqliteConn db =
  interpret \(Sql m) -> runNoLoggingT $ withSqliteConn db $
    MTL.lift . toEff . MTL.runReaderT m
