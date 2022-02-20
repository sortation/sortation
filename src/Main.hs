{-# options -fno-warn-orphans #-}
module Main where

-- import Cleff
-- import Cleff.Mask
-- import Cleff.Path
-- import Cleff.Reader
-- import Cleff.Sql
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Primitive
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Conduit.Combinators qualified as Conduit
import Database.Persist.Sqlite
import Effectful
import Effectful.Database
import Effectful.Reader.Static
import Optics
import Options.Applicative (execParser)
-- import Sortation.Check
import Sortation.Config
import Sortation.Library
import Sortation.Ingest.Config qualified as Ingest
import Sortation.Ingest.Dat
import Sortation.Ingest.Library
import Sortation.Persistent
import Sortation.Report.Text
import System.Path as Path
import System.Path.IO as Path
import Sortation.Format.Dat.Parse
import Text.XML.Stream.Parse qualified as XML

-- instance PrimMonad m => PrimMonad (LoggingT m) where
--   type PrimState (LoggingT m) = PrimState m
--   primitive = lift . primitive
-- 
-- instance PrimMonad m => PrimMonad (NoLoggingT m) where
--   type PrimState (NoLoggingT m) = PrimState m
--   primitive = lift . primitive

main :: IO ()
main = do
  command <- execParser configParserInfo
  runEff $ runDatabase @(Library "db")
    case command of
      Ingest config -> runReader config $ ingestLibrary @(Library "db")
      _ -> undefined

  -- runIOE $ runMask $ runSilentSqliteConn "db.sqlite" do
  --   sql $ runMigration migrateAll
  --   case command of
  --     Ingest config -> runReader config ingestDat
  --     Report -> fun -- printRomSets
  --     _ -> undefined

      -- Ingest config -> runReader config $
      --   bracket
      --     do liftIO . flip openFile ReadMode =<< normalizeFile (config ^. #datFile)
      --     do liftIO . hClose
      --     do
      --       \handle -> runConduit $
      --         Conduit.sourceHandle handle
      --           .| XML.parseBytes XML.def
      --           .| parseDat (const (pure ()))
      --           .| persistDat

      -- Report -> printCollections

      -- Check config -> runReader config checkCollection
