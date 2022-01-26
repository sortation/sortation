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
    flip runReaderT config $
    runConduit $
      Conduit.sourceFile (config ^. #datFile)
        .| XML.parseBytes def
        .| parseDat' reportDat
        .| Conduit.stdout
