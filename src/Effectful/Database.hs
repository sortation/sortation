module Effectful.Database where

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad.Catch
import Control.Natural
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.CasMap (CasMap, CasKey)
import Data.CasMap qualified as CasMap
import Data.Hashable
import Data.Kind
import Data.Serialize
import Effectful
import Effectful.Dispatch.Static
import GHC.Generics
import GHC.Prim
import GHC.TypeLits
import Optics
import System.Directory
import System.FilePath

type Key = CasKey

type BackendFileName :: forall k. k -> Symbol
type family BackendFileName db

type instance BackendFileName (path :: Symbol) = path

type HasBackend :: forall k. k -> Constraint
class Serialize (Backend db) => HasBackend db where
  type Backend db :: Type
  type TableOpticKind db :: OpticKind
  newBackend :: Backend db

class (HasBackend db, Hashable a) => HasTable db a where
  tableOptic :: Optic' (TableOpticKind db) NoIx (Backend db) (CasMap db a)

data Database :: forall k. k -> Effect

type instance DispatchOf (Database db) = Static WithSideEffects
newtype instance StaticRep (Database db) = Database { backendVar :: TVar (Backend db) }

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
  , Is (TableOpticKind db) A_Getter
  , Is (TableOpticKind db) A_Setter
  , HasTable db a
  ) =>
  a -> Eff es (Key db a)
insert x = do
  backendVar <- fmap backendVar $ getStaticRep @(Database db)
  map <- fmap (view tableOptic) $ unsafeEff_ $ atomically $ readTVar backendVar
  let (key, map') = CasMap.insert map x
  unsafeEff_ $ atomically $ modifyTVar' backendVar (tableOptic .~ map')
  pure key

index ::
  forall db a es.
  ( Database db :> es
  , Is (TableOpticKind db) A_Getter
  , HasTable db a
  ) =>
  Key db a -> Eff es a
index i = do
  backendVar <- fmap backendVar $ getStaticRep @(Database db)
  map <- fmap (view tableOptic) $ unsafeEff_ $ atomically $ readTVar backendVar
  pure $ CasMap.index map i
