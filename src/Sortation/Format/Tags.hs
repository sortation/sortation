module Sortation.Format.Tags where

import Data.Attoparsec.Text
import Data.Char
import Data.LanguageCodes

data Version = VersionNumber (Maybe ISO639_1) Text | Revision Text
  deriving (Generic, Eq, Ord, Show, Read)

data DevStatus = Beta Word | Proto | Sample
  deriving (Generic, Eq, Ord, Show, Read)

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
