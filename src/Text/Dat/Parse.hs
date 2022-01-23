module Text.Dat.Parse where

import Control.Monad.Catch
import Control.Monad.Reader
import Data.Conduit
import Data.String
import Data.Text qualified as Text
import Data.Text.Read qualified as Text
import Data.XML.Types qualified as XML
import Text.Dat
import Text.Hex
import Text.Read (readMaybe)
import Text.XML.Stream.Parse

parseDat' ::
  MonadThrow m =>
  ConduitT Game o (HeaderT m) r ->
  ConduitT XML.Event o m r
parseDat' processDat =
  force "datafile" $ tag' "datafile" ignoreAttrs \() -> do
    header <- parseHeader
    runHeaderC header (manyYield parseGame .| processDat)
    -- (lift . handleHeader =<< parseHeader) <* manyYield parseGame

parseDat ::
  MonadThrow m =>
  (Maybe Header -> m r) ->
  ConduitT XML.Event Game m r
parseDat handleHeader =
  force "datafile" $ tag' "datafile" ignoreAttrs \() ->
    (lift . handleHeader =<< parseHeader) <* manyYield parseGame

data DatException
  = ParseError Text
  deriving (Show, Eq, Ord)

instance Exception DatException

parseHex :: forall a m. (MonadThrow m, Integral a) => Text -> m a
parseHex text =
  case Text.hexadecimal text of
    Left err -> throwM $ ParseError $ Text.pack err
    Right (x, "") -> pure x
    Right (_, rest) -> throwM $ ParseError ("unexpected end of input: " <> rest)

parseHexByteString :: MonadThrow m => Text -> m ByteString
parseHexByteString text =
  case decodeHex text of
    Nothing -> throwM $ ParseError "bad hex string"
    Just x -> pure x

emptyTag :: MonadThrow m => NameMatcher a -> AttrParser b -> ConduitT XML.Event o m (Maybe b)
emptyTag name attrParser = tag' name attrParser pure

textTag :: MonadThrow m => String -> ConduitT XML.Event o m (Maybe Text)
textTag tagName = tag' (fromString tagName) ignoreAttrs \() -> content

requireTextTag :: MonadThrow m => String -> ConduitT XML.Event o m Text
requireTextTag tagName = force tagName $ textTag tagName

parseHeader :: MonadThrow m => ConduitT XML.Event o m (Maybe Header)
parseHeader =
  tag' "header" ignoreAttrs \() -> do
    name <- requireTextTag "name"
    description <- requireTextTag "description"
    category <- textTag "category"
    version <- requireTextTag "version"
    date <- textTag "date"
    author <- requireTextTag "author"
    email <- textTag "email"
    homepage <- textTag "homepage"
    url <- textTag "url"
    comment <- textTag "comment"
    clrMamePro <- parseClrMameProHeader
    romCenter <- parseRomCenterHeader
    pure Header { .. }

parseClrMameProHeader :: MonadThrow m => ConduitT XML.Event o m (Maybe ClrMameProHeader)
parseClrMameProHeader =
  emptyTag "clrmamepro" do
    header <- attr "header"
    forceMerging <- maybe (pure SplitMerge) parseMergeType =<< attr "forcemerging"
    forceNoDump <- maybe (pure Obsolete) parseNoDumpType =<< attr "forcenodump"
    forcePacking <- maybe (pure Zip) parsePackingType =<< attr "forcepacking"
    pure ClrMameProHeader { .. }

parseRomCenterHeader :: MonadThrow m => ConduitT XML.Event o m (Maybe RomCenterHeader)
parseRomCenterHeader =
  emptyTag "romcenter" do
    plugin <- attr "plugin"
    romMode <- maybe (pure Split) parseRomSetType =<< attr "rommode"
    biosMode <- maybe (pure Split) parseRomSetType =<< attr "biosmode"
    sampleMode <- maybe (pure MergedSamples) parseSampleSetType =<< attr "samplemode"
    lockRomMode <- maybe (pure False) parseYesNo =<< attr "lockrommode"
    lockBiosMode <- maybe (pure False) parseYesNo =<< attr "lockbiosmode"
    lockSampleMode <- maybe (pure False) parseYesNo =<< attr "locksamplemode"
    pure RomCenterHeader { .. }

parseMergeType :: MonadThrow m => Text -> m MergeType
parseMergeType input =
  case input of
    "none" -> pure NoMerge
    "split" -> pure SplitMerge
    "full" -> pure FullMerge
    _ -> throwM $ ParseError ("invalid forcemerging: " <> input)

parseNoDumpType :: MonadThrow m => Text -> m NoDumpType
parseNoDumpType input =
  case input of
    "obsolete" -> pure Obsolete
    "required" -> pure Required
    "ignore" -> pure Ignore
    _ -> throwM $ ParseError ("invalid forcenodump: " <> input)

parsePackingType :: MonadThrow m => Text -> m PackingType
parsePackingType input =
  case input of
    "zip" -> pure Zip
    "unzip" -> pure Unzip
    _ -> throwM $ ParseError ("invalid forcepacking: " <> input)

parseRomSetType :: MonadThrow m => Text -> m RomSetType
parseRomSetType input =
  case input of
    "merged" -> pure Merged
    "split" -> pure Split
    "unmerged" -> pure Unmerged
    _ -> throwM $ ParseError ("invalid rommode/biosmode: " <> input)

parseSampleSetType :: MonadThrow m => Text -> m SampleSetType
parseSampleSetType input =
  case input of
    "merged" -> pure MergedSamples
    "unmerged" -> pure UnmergedSamples
    _ -> throwM $ ParseError ("invalid samplemode: " <> input)

parseYesNo :: MonadThrow m => Text -> m Bool
parseYesNo input =
  case input of
    "yes" -> pure True
    "no" -> pure False
    _ -> throwM $ ParseError ("invalid yes/no value: " <> input)

parseGame :: MonadThrow m => ConduitT XML.Event o m (Maybe Game)
parseGame =
  tag' "game"
    do
      name <- requireAttr "name"
      sourceFile <- attr "sourcefile"
      isBios <- (maybe (pure False) parseYesNo =<< attr "isbios")
      cloneOf <- attr "cloneof"
      romOf <- attr "romof"
      sampleOf <- attr "sampleof"
      board <- attr "board"
      rebuildTo <- attr "rebuildto"
      pure GameAttrs { .. }
    \GameAttrs { .. } -> do
      comments <- many (textTag "comment")
      description <- requireTextTag "description"
      year <- textTag "year"
      manufacturer <- textTag "manufacturer"
      releases <- many parseRelease
      biosSets <- many parseBiosSet
      roms <- many parseRom
      disks <- many parseDisk
      samples <- many parseSample
      archives <- many parseArchive
      pure Game { .. }
  where

data GameAttrs = GameAttrs
  { name :: Text
  , sourceFile :: Maybe Text
  , isBios :: Bool
  , cloneOf :: Maybe Text
  , romOf :: Maybe Text
  , sampleOf :: Maybe Text
  , board :: Maybe Text
  , rebuildTo :: Maybe Text
  }

parseRelease :: MonadThrow m => ConduitT XML.Event o m (Maybe Release)
parseRelease =
  emptyTag "release" do
    name <- requireAttr "name"
    region <- requireAttr "region"
    language <- attr "language"
    date <- attr "date"
    _default <- maybe (pure False) parseYesNo =<< attr "_default"
    pure Release { .. }

parseBiosSet :: MonadThrow m => ConduitT XML.Event o m (Maybe BiosSet)
parseBiosSet =
  emptyTag "biosset" do
    name <- requireAttr "name"
    description <- requireAttr "description"
    _default <- maybe (pure False) parseYesNo =<< attr "_default"
    pure BiosSet { .. }

parseRom :: MonadThrow m => ConduitT XML.Event o m (Maybe Rom)
parseRom =
  emptyTag "rom" do
    name <- requireAttr "name"
    size <- force "size" $ readMaybe . Text.unpack <$> requireAttr "size"
    crc <- traverse parseHex =<< attr "crc"
    sha1 <- traverse parseHexByteString =<< attr "sha1"
    md5 <- traverse parseHexByteString =<< attr "md5"
    merge <- attr "merge"
    status <- maybe (pure Good) parseRomStatus =<< attr "status"
    date <- attr "date"
    pure Rom { .. }

parseDisk :: MonadThrow m => ConduitT XML.Event o m (Maybe Disk)
parseDisk =
  emptyTag "disk" do
    name <- requireAttr "name"
    sha1 <- traverse parseHex =<< attr "sha1"
    md5 <- traverse parseHex =<< attr "md5"
    merge <- attr "merge"
    status <- maybe (pure Good) parseRomStatus =<< attr "status"
    pure Disk { .. }

parseRomStatus :: MonadThrow m => Text -> m RomStatus
parseRomStatus input =
  case input of
    "baddump" -> pure BadDump
    "nodump" -> pure NoDump
    "good" -> pure Good
    "verified" -> pure Verified
    _ -> throwM $ ParseError ("invalid status: " <> input)

parseSample :: MonadThrow m => ConduitT XML.Event o m (Maybe Sample)
parseSample =
  emptyTag "sample" do
    name <- requireAttr "name"
    pure Sample { .. }

parseArchive :: MonadThrow m => ConduitT XML.Event o m (Maybe Archive)
parseArchive =
  emptyTag "archive" do
    name <- requireAttr "name"
    pure Archive { .. }
