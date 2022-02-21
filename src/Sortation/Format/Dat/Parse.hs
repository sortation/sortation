module Sortation.Format.Dat.Parse where

import Data.Map qualified as Map
import Data.Vector qualified as Vector
import Data.Text.Read qualified as Text
import Effectful.Consume
import Sortation.Format.Dat
import Text.Hex (decodeHex)
import Text.XML qualified as XML

makeFieldLabels ''XML.Document
makeFieldLabels ''XML.Prologue
makeFieldLabels ''XML.Instruction
makePrisms ''XML.Miscellaneous
makePrisms ''XML.Node
makeFieldLabels ''XML.Element
makeFieldLabels ''XML.Name
makeFieldLabels ''XML.Doctype
makePrisms ''XML.ExternalID

data DatParseError = DatParseError { message :: Text }
  deriving (Generic, Exception, Show, Read, Eq, Ord)

readDat :: IOE :> es => FilePath -> Eff es Dat
readDat path = do
  doc <- liftIO (XML.readFile XML.def path)
  runConsume (Identity doc.root) parseDat

type Attr = (XML.Name, Text)

parseDat :: Eff (Consume XML.Element : es) Dat
parseDat = do
  datafile <- requireTag "datafile"
  runConsume (datafile ^.. (#nodes % traversed % _NodeElement)) do
    header <- parseHeader
    games <- Vector.fromList <$> unfoldM parseGame
    pure Dat { .. }

parseHex :: forall a es. Integral a => Text -> Eff es a
parseHex text =
  case Text.hexadecimal text of
    Left err -> throwM $ DatParseError $ pack err
    Right (x, "") -> pure x
    Right (_, rest) -> throwM $ DatParseError ("unexpected end of input: " <> rest)

parseHexByteString :: Text -> Eff es ByteString
parseHexByteString text =
  case decodeHex text of
    Nothing -> throwM $ DatParseError "bad hex string"
    Just x -> pure x

parseHeader :: Eff (Consume XML.Element : es) (Maybe Header)
parseHeader = do
  optionTag "header" >>= traverse \node -> runConsume (node ^.. #nodes % traversed % _NodeElement) do
    name <- requireTextTag "name"
    description <- requireTextTag "description"
    category <- optionTextTag "category"
    version <- requireTextTag "version"
    date <- optionTextTag "date"
    author <- requireTextTag "author"
    email <- optionTextTag "email"
    homepage <- optionTextTag "homepage"
    url <- optionTextTag "url"
    comment <- optionTextTag "comment"
    clrMamePro <- parseClrMameProHeader
    romCenter <- parseRomCenterHeader
    pure Header { .. }

parseClrMameProHeader :: Eff (Consume XML.Element : es) (Maybe ClrMameProHeader)
parseClrMameProHeader =
  optionEmptyTag "clrmamepro" do
    forceMerging <- maybe (pure SplitMerge) parseMergeType =<< optionAttr "forcemerging"
    forceNoDump <- maybe (pure Obsolete) parseNoDumpType =<< optionAttr "forcenodump"
    forcePacking <- maybe (pure Zip) parsePackingType =<< optionAttr "forcepacking"
    header <- optionAttr "header"
    pure ClrMameProHeader { .. }

parseRomCenterHeader :: Eff (Consume XML.Element : es) (Maybe RomCenterHeader)
parseRomCenterHeader =
  optionEmptyTag "romcenter" do
    biosMode <- maybe (pure Split) parseRomSetType =<< optionAttr "biosmode"
    lockBiosMode <- maybe (pure False) parseYesNo =<< optionAttr "lockbiosmode"
    lockRomMode <- maybe (pure False) parseYesNo =<< optionAttr "lockrommode"
    lockSampleMode <- maybe (pure False) parseYesNo =<< optionAttr "locksamplemode"
    plugin <- optionAttr "plugin"
    romMode <- maybe (pure Split) parseRomSetType =<< optionAttr "rommode"
    sampleMode <- maybe (pure MergedSamples) parseSampleSetType =<< optionAttr "samplemode"
    pure RomCenterHeader { .. }

parseMergeType :: Text -> Eff es MergeType
parseMergeType input =
  case input of
    "none" -> pure NoMerge
    "split" -> pure SplitMerge
    "full" -> pure FullMerge
    _ -> throwM $ DatParseError ("invalid forcemerging: " <> input)

parseNoDumpType :: Text -> Eff es NoDumpType
parseNoDumpType input =
  case input of
    "obsolete" -> pure Obsolete
    "required" -> pure Required
    "ignore" -> pure Ignore
    _ -> throwM $ DatParseError ("invalid forcenodump: " <> input)

parsePackingType :: Text -> Eff es PackingType
parsePackingType input =
  case input of
    "zip" -> pure Zip
    "unzip" -> pure Unzip
    _ -> throwM $ DatParseError ("invalid forcepacking: " <> input)

parseRomSetType :: Text -> Eff es RomSetType
parseRomSetType input =
  case input of
    "merged" -> pure Merged
    "split" -> pure Split
    "unmerged" -> pure Unmerged
    _ -> throwM $ DatParseError ("invalid rommode/biosmode: " <> input)

parseSampleSetType :: Text -> Eff es SampleSetType
parseSampleSetType input =
  case input of
    "merged" -> pure MergedSamples
    "unmerged" -> pure UnmergedSamples
    _ -> throwM $ DatParseError ("invalid samplemode: " <> input)

parseYesNo :: Text -> Eff es Bool
parseYesNo input =
  case input of
    "yes" -> pure True
    "no" -> pure False
    _ -> throwM $ DatParseError ("invalid yes/no value: " <> input)

parseGame :: Eff (Consume XML.Element : es) (Maybe Game)
parseGame =
  optionTag "game" >>= traverse \node -> do
    GameAttrs { .. } <-
      runConsume (node & itoListOf (#attributes % itraversed)) do
        board <- optionAttr "board"
        cloneOf <- optionAttr "cloneof"
        isBios <- (maybe (pure False) parseYesNo =<< optionAttr "isbios")
        name <- requireAttr "name"
        rebuildTo <- optionAttr "rebuildto"
        romOf <- optionAttr "romof"
        sampleOf <- optionAttr "sampleof"
        sourceFile <- optionAttr "sourcefile"
        pure GameAttrs { .. }
    runConsume (node ^.. #nodes % traversed % _NodeElement) do
      comments <- fmap Vector.fromList $ unfoldM $ optionTextTag "comment"
      description <- requireTextTag "description"
      identifier <- optionTextTag "game_id"
      year <- optionTextTag "year"
      manufacturer <- optionTextTag "manufacturer"
      releases <- fmap Vector.fromList $ unfoldM parseRelease
      biosSets <- fmap Vector.fromList $ unfoldM parseBiosSet
      roms <- fmap Vector.fromList $ unfoldM parseRom
      disks <- fmap Vector.fromList $ unfoldM parseDisk
      samples <- fmap Vector.fromList $ unfoldM parseSample
      archives <- fmap Vector.fromList $ unfoldM parseArchive
      pure Game { .. }

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

parseRelease :: Eff (Consume XML.Element : es) (Maybe Release)
parseRelease =
  optionEmptyTag "release" do
    date <- optionAttr "date"
    language <- optionAttr "language"
    name <- requireAttr "name"
    region <- requireAttr "region"
    _default <- maybe (pure False) parseYesNo =<< optionAttr "default"
    pure Release { .. }

parseBiosSet :: Eff (Consume XML.Element : es) (Maybe BiosSet)
parseBiosSet =
  optionEmptyTag "biosset" do
    _default <- maybe (pure False) parseYesNo =<< optionAttr "default"
    description <- requireAttr "description"
    name <- requireAttr "name"
    pure BiosSet { .. }

parseRom :: Eff (Consume XML.Element : es) (Maybe Rom)
parseRom =
  optionEmptyTag "rom" do
    crc <- traverse parseHex =<< optionAttr "crc"
    date <- optionAttr "date"
    md5 <- traverse parseHexByteString =<< optionAttr "md5"
    merge <- optionAttr "merge"
    name <- requireAttr "name"
    serial <- optionAttr "serial"
    sha1 <- traverse parseHexByteString =<< optionAttr "sha1"
    size <- read . unpack <$> requireAttr "size"
    status <- maybe (pure Good) parseRomStatus =<< optionAttr "status"
    pure Rom { .. }

parseDisk :: Eff (Consume XML.Element : es) (Maybe Disk)
parseDisk =
  optionEmptyTag "biosset" do
    md5 <- traverse parseHex =<< optionAttr "md5"
    merge <- optionAttr "merge"
    name <- requireAttr "name"
    sha1 <- traverse parseHex =<< optionAttr "sha1"
    status <- maybe (pure Good) parseRomStatus =<< optionAttr "status"
    pure Disk { .. }

parseRomStatus :: Text -> Eff es RomStatus
parseRomStatus input =
  case input of
    "baddump" -> pure BadDump
    "nodump" -> pure NoDump
    "good" -> pure Good
    "verified" -> pure Verified
    _ -> throwM $ DatParseError ("invalid status: " <> input)

parseSample :: Eff (Consume XML.Element : es) (Maybe Sample)
parseSample =
  optionEmptyTag "sample" do
    name <- requireAttr "name"
    pure Sample { .. }

parseArchive :: Eff (Consume XML.Element : es) (Maybe Archive)
parseArchive =
  optionEmptyTag "archive" do
    name <- requireAttr "name"
    pure Archive { .. }

requireTag :: Consume XML.Element :> es => Text -> Eff es XML.Element
requireTag expected =
  consume @XML.Element >>= \case
    Nothing -> throwM $ DatParseError ("expected tag " <> expected)
    Just node ->
      if node.name.localName == expected then
        pure node
      else
        throwM $ DatParseError ("expected tag " <> expected <> ", got " <> node.name.localName)

requireTextTag :: Consume XML.Element :> es => Text -> Eff es Text
requireTextTag expected =
  toListOf (#nodes % traversed) <$> requireTag expected >>= \case
    [XML.NodeContent text] -> pure text
    tags -> throwM $ DatParseError ("expected text, got " <> pack (show tags))

optionTag :: Consume XML.Element :> es => Text -> Eff es (Maybe XML.Element)
optionTag expected =
  peek >>= \case
    Just node | node.name.localName == expected -> consume @XML.Element $> Just node
    _ -> pure Nothing

optionEmptyTag :: Consume XML.Element :> es => Text -> Eff (Consume Attr : es) a -> Eff es (Maybe a)
optionEmptyTag expected m =
  optionTag expected >>= traverse \element -> do
    runConsume element.nodes $ assertEnd @XML.Node
    runConsume (Map.toList element.attributes) m

optionTextTag :: Consume XML.Element :> es => Text -> Eff es (Maybe Text)
optionTextTag expected =
  fmap (toListOf (#nodes % traversed)) <$> optionTag expected >>= \case
    Nothing -> pure Nothing
    Just [XML.NodeContent text] -> pure $ Just text
    Just tags -> throwM $ DatParseError ("expected text, got " <> pack (show tags))

requireAttr :: Consume Attr :> es => Text -> Eff es Text
requireAttr expected =
  consume @Attr >>= \case
    Nothing -> throwM $ DatParseError ("expected attr " <> expected)
    Just (optionAttr, value) ->
      if optionAttr.localName == expected then
        pure value
      else
        throwM $ DatParseError ("expected attr " <> expected <> ", got " <> optionAttr.localName)

optionAttr :: Consume Attr :> es => Text -> Eff es (Maybe Text)
optionAttr expected =
  peek @Attr >>= \case
    Just (optionAttr, value) | optionAttr.localName == expected -> consume @Attr $> Just value
    _ -> pure Nothing

assertEnd :: forall a es. (Show a, Consume a :> es) => Eff es ()
assertEnd =
  consume @a >>= \case
    Nothing -> pure ()
    Just x ->
      throwM $ DatParseError ("expected end, got " <> pack (show x))
