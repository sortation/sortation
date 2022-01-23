module Main where

import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Conduit.Combinators qualified as Conduit
import Optics
import Options.Applicative
import Sortation.Check
import Sortation.Config
import Text.Dat.Parse
import Text.XML.Stream.Parse as XML

main :: IO ()
main = do
  config <- execParser optionsParser
  runResourceT $
    flip runReaderT
      ( config
          & #romDirectory .~ "/data/serving/software/no-intro/Tiger - Game.com"
      ) $
    runConduit $
      Conduit.sourceFile "/data/serving/software/dats/Tiger - Game.com (20081125-110950).dat"
        .| XML.parseBytes def
        .| parseDat' reportDat
        .| Conduit.stdout
