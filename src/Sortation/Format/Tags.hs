module Sortation.Format.Tags where

import Control.Applicative
import Control.Monad.Loops
import Data.Attoparsec.Text
import Data.Char
import Data.LanguageCodes
import Data.Text (Text)
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics

data Version = VersionNumber (Maybe ISO639_1) Text | Revision Text
  deriving (Generic, Eq, Ord, Show, Read)

data DevStatus = Beta Word | Proto | Sample
  deriving (Generic, Eq, Ord, Show, Read)

derivePersistField "ISO639_1"
derivePersistField "Version"
derivePersistField "DevStatus"

parseISO639_1 :: Parser ISO639_1
parseISO639_1 =
  liftA2 fromChars (toLower <$> letter) (toLower <$> letter) >>= \case
    Nothing -> fail "invalid language code"
    Just code -> pure code

parseVersion :: Parser Version
parseVersion =
  choice
    [ VersionNumber <$> optional (parseISO639_1 <* char ' ') <*> (char 'v' *> takeWhile1 (orM [isDigit, (==) '.']))
    , Revision <$> ("Rev " *> takeWhile1 isAlphaNum)
    ]

parseDevStatus :: Parser DevStatus
parseDevStatus =
  choice
    [ Beta . read <$> ("Beta " *> many1 digit)
    , Beta 1 <$ "Beta"
    , Proto <$ "Proto"
    , Sample <$ "Sample"
    ]
