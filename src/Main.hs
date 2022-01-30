{-# options -fno-warn-orphans #-}
module Main where

import Control.Monad.Logger
import Control.Monad.Primitive
import Control.Monad.Reader
import Data.Pool
import Database.Persist.Monad
import Database.Persist.Sqlite (withSqlitePool)
import Optics
import Options.Applicative
import Sortation.Config
import Sortation.Ingest.Dat
import Sortation.Persistent
import Sortation.Report.Text

instance PrimMonad m => PrimMonad (SqlQueryT m) where
  type PrimState (SqlQueryT m) = PrimState m
  primitive = lift . primitive

sqliteConnections :: Int
sqliteConnections = 8

main :: IO ()
main =  do
  config <- execParser optionsParser
  runNoLoggingT $ withSqlitePool "db.sqlite" sqliteConnections \sql -> do
    liftIO $ runSqlQueryT sql do
      runMigration migrateAll
      case config of
        Check c -> do
          (_, games) <- parseDatVec =<< parsePath (c ^. #globalConfig % #datFile)
          void $ persistDat "test" games
    withResource sql $ runReaderT printCollections
