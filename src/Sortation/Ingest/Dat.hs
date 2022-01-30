module Sortation.Ingest.Dat where

import Control.Monad.Primitive
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Conduit.Combinators qualified as Conduit
import Data.Foldable
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Database.Persist.Monad
import Optics
import Sortation.Persistent qualified as Persistent
import System.Path qualified as Path
import Text.Dat qualified as Dat
import Text.Dat.Parse
import Text.XML.Stream.Parse qualified as XML

parseDatVec ::
  (MonadThrow m, MonadUnliftIO m, PrimMonad m) =>
  Path.AbsFile ->
  m (Maybe Dat.Header, Vector Dat.Game)
parseDatVec datFile =
  runConduitRes $
    Conduit.sourceFile (Path.toString datFile)
      .| XML.parseBytes XML.def
      .| parseDat \header -> (header,) <$> Conduit.sinkVector

persistDat :: MonadSqlQuery m => Text -> Vector Dat.Game -> m ()
persistDat collectionName games = do
  collectionId <- insert Persistent.Collection
    { name = collectionName
    , parents = Set.empty
    }
  withTransaction $ traverse_ (persistGame collectionId) games

persistGame :: MonadSqlQuery m => Persistent.CollectionId -> Dat.Game -> m ()
persistGame collectionId game = do
  gameId <- insert Persistent.Game
    { parents = Set.singleton collectionId
    , name = game ^. #name
    }
  releaseId <- insert Persistent.Release
    { parent = gameId
    , region = Nothing
    , language = Nothing
    , version = Nothing
    , date = Nothing
    }
  traverse_ (persistRom releaseId) (NonEmpty.fromList (game ^. #roms))

persistRom :: MonadSqlQuery m => Persistent.ReleaseId -> Dat.Rom -> m ()
persistRom releaseId rom =
 insert_ Persistent.Rom
   { parent = releaseId
   , name = rom ^. #name
   , path = "TODO"
   , size = rom ^. #size
   , crc = rom ^. #crc
   , md5 = rom ^. #md5
   , sha1 = rom ^. #sha1
   }
