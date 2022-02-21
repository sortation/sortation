module Sortation.Format.NoIntro where

import Data.Attoparsec.Text
import Data.Char
import Data.LanguageCodes
import Data.List qualified as List
import Data.Sequence (ViewR(..), viewr)
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as Text
import Sortation.Format.Tags

data Title = Title
  { name :: Text
  , article :: Maybe Text
  , subtitles :: [Text]
  , bios :: Bool
  , titleId :: Maybe Text
  , regions :: Set Text
  , languages :: Set ISO639_1
  , version :: Maybe Version
  , devStatus :: Maybe DevStatus
  , disc :: Maybe Text
  , misc :: [Text]
  , unlicensed :: Bool
  , bad :: Bool
  } deriving (Generic, Eq, Ord, Show)

parseTitle :: Parser Title
parseTitle = do
  bios <- option False ("[BIOS] " $> True)
  (name, article) <- parseTitleName
  subtitles <- many (" - " *> parseSubtitle)
  titleId <- optional (" (" *> takeWhile1 isHexDigit <* ")")
  regions <- Set.fromList <$> (" (" *> sepBy1 parseWord ", " <* ")")
  languages <-
    option Set.empty $ Set.fromList <$>
      (" (" *> sepBy1 parseISO639_1 "," <* ")")
  version <- optional (" (" *> parseVersion <* ")")
  disc <- optional (" (Disc " *> takeWhile1 isAlphaNum <* ")")
  devStatus <- optional (" (" *> parseDevStatus <* ")")
  (misc, unlicensed) <-
    viewr . Seq.fromList <$> many (" (" *> parseSubtitle <* ")") <&> \case
      EmptyR -> ([], False)
      misc :> "Unl" -> (toList misc, True)
      misc -> (toList misc, False)
  bad <- option False (" [b]" $> True)
  endOfInput
  pure Title { .. }

parseWord :: Parser Text
parseWord = takeWhile1 isLetter

parseSubtitle :: Parser Text
parseSubtitle =
  fmap (mconcat . List.intersperse " ") $ flip sepBy1 (char ' ') $ takeWhile1 $ orM
    [ isAlphaNum
    , flip elem ("&$!#%'+,-.;=@[]^_{}~" :: [Char])
    ]

parseTitleName :: Parser (Text, Maybe Text)
parseTitleName = do
  subtitle <- parseSubtitle
  let parts = reverse $ Text.splitOn ", " subtitle
  case parts of
    (article : rest@(_ : _)) ->
      pure (mconcat (reverse (List.intersperse ", " rest)), Just article)
    _ -> pure (mconcat (reverse (List.intersperse ", " parts)), Nothing)
