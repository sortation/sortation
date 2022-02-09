module Sortation.Config where

import GHC.Generics
import Options.Applicative
import Sortation.Ingest.Config qualified as Ingest
import Sortation.Check.Config qualified as Check

data Config
  = Ingest Ingest.Config
  | Report
  | Check Check.Config

configParserInfo :: ParserInfo Config
configParserInfo =
  info (configParser <**> helper) $ mconcat
    [ fullDesc
    , header "sortation"
    ]

configParser :: Parser Config
configParser =
  hsubparser $ mconcat
    [ command "ingest" (Ingest <$> Ingest.configParserInfo)
    , command "report" (info (pure Report) mempty)
    , command "check" (Check <$> Check.configParserInfo)
    ]
