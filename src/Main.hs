module Main where

import Effectful.Database
import Options.Applicative (execParser)
import Sortation.Config
import Sortation.Library
import Sortation.Ingest.Dat
import Sortation.Report.Text

main :: IO ()
main = do
  command <- execParser configParserInfo
  runEff $ runDatabase @(Library "db") "."
    case command of
      Ingest config -> runReader config $ ingest @"db"
      Report -> reportLibrary @"db"
      _ -> undefined
