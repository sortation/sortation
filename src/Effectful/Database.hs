module Effectful.Database where

import Data.ByteString qualified as ByteString
import Data.CasMap (CasMap, CasKey)
import Data.CasMap qualified as CasMap
import Data.Hashable
import Data.Serialize
import Data.Vector qualified as Vector
import Effectful.Dispatch.Static
import System.Directory
import System.FilePath

type Key = CasKey

type BackendFileName :: forall k. k -> Symbol
type family BackendFileName db

type instance BackendFileName (path :: Symbol) = path

type HasBackend :: forall k. k -> Constraint
class Serialize (Backend db) => HasBackend db where
  type Backend db :: Type
  type BackendOpticKind db :: OpticKind
  newBackend :: Backend db

type ReadableDatabase :: forall k. k -> Constraint
type ReadableDatabase db = Is (BackendOpticKind db) A_Getter

type ModifiableDatabase :: forall k. k -> Constraint
type ModifiableDatabase db = (ReadableDatabase db, Is (BackendOpticKind db) A_Setter)

class (HasBackend db, Hashable a) => HasTable db a where
  tableOptic :: Optic' (BackendOpticKind db) NoIx (Backend db) (CasMap db a)

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

insert ::
  forall db a es.
  ( Database db :> es
  , ModifiableDatabase db
  , HasTable db a
  ) =>
  a -> Eff es (Key db a)
insert x = do
  backendVar <- fmap (.backendVar) $ getStaticRep @(Database db)
  map <- fmap (view tableOptic) $ unsafeEff_ $ readTVarIO backendVar
  let (key, map') = CasMap.insert map x
  unsafeEff_ $ atomically $ modifyTVar' backendVar (tableOptic .~ map')
  pure key

index ::
  forall db a es.
  ( Database db :> es
  , ReadableDatabase db
  , HasTable db a
  ) =>
  Key db a -> Eff es a
index i = do
  backendVar <- fmap (.backendVar) $ getStaticRep @(Database db)
  map <- fmap (view tableOptic) $ unsafeEff_ $ atomically $ readTVar backendVar
  pure $ CasMap.index map i

tableVector ::
  forall db a es.
  ( Database db :> es
  , ReadableDatabase db
  , HasTable db a
  ) =>
  Eff es (Vector a)
tableVector = do
  backendVar <- fmap (.backendVar) $ getStaticRep @(Database db)
  map <- fmap (view (tableOptic @db)) $ unsafeEff_ $ readTVarIO backendVar
  pure $ Vector.fromListN (length map) $ toList map
