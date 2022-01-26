module Main where

import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Conduit.Combinators qualified as Conduit
import Data.Conduit.Lift
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
    runConduit $
      case config of
        Check c ->
          Conduit.sourceFile (c ^. #globalConfig % #datFile)
            .| XML.parseBytes def
            .| runReaderC c (parseDat' reportDat)
            .| Conduit.stdout
