module Effectful.Database where

import Data.ByteString qualified as ByteString
import Data.IntMap.Strict (IntMap, (!))
import Data.IntMap.Strict qualified as IntMap
import Data.Serialize
import Data.Vector qualified as Vector
import Data.Vinyl.Core
import Data.Vinyl.Functor (Compose(..))
import Data.Vinyl.XRec
import Effectful.Dispatch.Static
import System.Directory
import System.FilePath

type Key :: forall k. k -> Type -> Type
newtype Key db a = Key { keyId :: IntMap.Key }
  deriving stock (Generic, Show, Read, Eq, Ord, Bounded)
  deriving anyclass Serialize
  deriving newtype (Num, Enum)

type BackendFileName :: forall k. k -> Symbol
type family BackendFileName db

type instance BackendFileName (path :: Symbol) = path

type HasBackend :: forall k. k -> Constraint
class Serialize (Backend db) => HasBackend db where
  data Backend db :: Type
  type BackendOpticKind db :: OpticKind
  newBackend :: Backend db

type ReadableDatabase :: forall k. k -> Constraint
type ReadableDatabase db = Is (BackendOpticKind db) A_Getter

type ModifiableDatabase :: forall k. k -> Constraint
type ModifiableDatabase db = (ReadableDatabase db, Is (BackendOpticKind db) A_Setter)

class HasBackend db => HasTable db a where
  tableOptic :: Optic' (BackendOpticKind db) NoIx (Backend db) (IntMap a)

data Database :: forall k. k -> Effect

type instance DispatchOf (Database db) = Static WithSideEffects
newtype instance StaticRep (Database db) = Database { backendVar :: TVar (Backend db) }
  deriving Generic

newtype DatabaseDeserializationError = DatabaseDeserializationError { message :: String }
  deriving stock (Generic, Eq, Ord, Show, Read)
  deriving anyclass Exception

runDatabase ::
  forall db es.
  (IOE :> es, HasBackend db, KnownSymbol (BackendFileName db)) =>
  FilePath -> Eff (Database db : es) ~> Eff es
runDatabase dir m = do
  let backendPath = dir </> symbolVal' @(BackendFileName db) proxy#
  backend <- liftIO (doesFileExist backendPath) >>= \case
    False -> pure $ newBackend @_ @db
    True ->
      decode <$> liftIO (ByteString.readFile backendPath) >>= \case
        Left err -> throwM $ DatabaseDeserializationError err
        Right backend -> pure backend
  backendVar <- unsafeEff_ $ newTVarIO backend
  r <- evalStaticRep (Database backendVar) m
  liftIO . ByteString.writeFile backendPath . encode =<< liftIO (readTVarIO backendVar)
  pure r

databaseRep ::
  forall db es.
  Database db :> es =>
  Eff es (TVar (Backend db))
databaseRep = getStaticRep @(Database db) ^. mapping #backendVar

nextKey :: forall db a. IntMap a -> Key db a
nextKey = Key . maybe minBound succ . fmap fst . IntMap.lookupMax

insert ::
  forall db a es.
  ( Database db :> es
  , ModifiableDatabase db
  , HasTable db a
  ) =>
  a -> Eff es (Key db a)
insert x = do
  backendVar <- databaseRep @db
  k <- fmap (nextKey . view tableOptic) $ unsafeEff_ $ readTVarIO backendVar
  unsafeEff_ $ atomically $ modifyTVar' backendVar $
    tableOptic @db %~ IntMap.insert k.keyId x
  pure k

delete ::
  forall db a es.
  ( Database db :> es
  , ModifiableDatabase db
  , HasTable db a
  ) =>
  Key db a -> Eff es ()
delete k = do
  backendVar <- databaseRep @db
  unsafeEff_ $ atomically $ modifyTVar' backendVar $
    tableOptic @db @a %~ IntMap.delete k.keyId

select ::
  forall db a es.
  ( Database db :> es
  , ReadableDatabase db
  , HasTable db a
  ) =>
  Key db a -> Eff es a
select k = do
  backendVar <- databaseRep @db
  map <- fmap (view tableOptic) $ unsafeEff_ $ atomically $ readTVar backendVar
  pure (map ! k.keyId)

selects ::
  forall db a es.
  ( Database db :> es
  , ReadableDatabase db
  , HasTable db a
  ) =>
  Vector (Key db a) -> Eff es (Vector a)
selects ks = do
  backendVar <- databaseRep @db
  map <- fmap (view tableOptic) $ unsafeEff_ $ atomically $ readTVar backendVar
  pure $ Vector.map (\k -> map ! k.keyId) ks

selectAllWithKeys ::
  forall db a es.
  ( Database db :> es
  , ReadableDatabase db
  , HasTable db a
  ) =>
  Eff es (Vector (Key db a, a))
selectAllWithKeys = do
  backendVar <- databaseRep @db
  map <- fmap (view tableOptic) $ unsafeEff_ $ atomically $ readTVar backendVar
  pure $ fmap (first Key) $ Vector.fromListN (length map) $ IntMap.toList map

selectAll ::
  forall db a es.
  ( Database db :> es
  , ReadableDatabase db
  , HasTable db a
  ) =>
  Eff es (Vector a)
selectAll = fmap (fmap snd) $ selectAllWithKeys @db

type Join :: forall k. k -> Type -> [Type] -> Constraint
class Join db j as | j -> db as where
  joinKeys :: j -> Rec (Key db) as
  default joinKeys :: Coercible j (Rec (Key db) as) => j -> Rec (Key db) as
  joinKeys = coerce

type KeyType :: Type -> Type
type family KeyType k where
  KeyType (Key db a) = a

type HasTableForKey :: forall k. k -> Type -> Constraint
class HasTable db (KeyType k) => HasTableForKey db k where
instance HasTable db (KeyType k) => HasTableForKey db k where

selectJoin ::
  forall db j as es.
  ( Database db :> es
  , ReadableDatabase db
  , Join db j as
  , HasTable db j
  , ReifyConstraint (HasTableForKey db) (Key db) as
  , IsoXRec Identity as, TupleXRec Identity as
  ) =>
  Eff es (Vector (ListToHKDTuple Identity as))
selectJoin = do
  ks <- fmap (fmap joinKeys) $ selectAll @db @j
  fmap (fmap (ruple @Identity @as)) $ for ks \rec ->
    rtraverse
      (\(Compose (Dict k)) -> Identity <$> select k)
      (reifyConstraint @(HasTableForKey db) rec)

-- compact ::
--   forall db a es.
--   ( Database db :> es
--   , ModifiableDatabase db
--   , HasTable db a
--   ) =>
--   Eff es ()
-- compact = do
--   backendVar <- databaseRep @db
--   unsafeEff_ $ atomically $ modifyTVar' backendVar $
--     tableOptic @db @a %~
--       IntMap.foldl' (\m x -> IntMap.insert (nextKey m).keyId x m) IntMap.empty
