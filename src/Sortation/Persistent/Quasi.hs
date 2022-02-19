module Sortation.Persistent.Quasi where

import Data.Coerce
import Data.List.NonEmpty (NonEmpty(..))
import Database.Persist
import Database.Persist.Quasi.Internal
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics
import Iso.Deriving
import Language.Haskell.TH.Syntax

instance (Isomorphic a b, PersistField a) => PersistField (As a b) where
  toPersistValue (As x) = toPersistValue $ prj @a @b x
  fromPersistValue = fmap (As . inj @a @b) . fromPersistValue

instance (Isomorphic a b, PersistFieldSql a) => PersistFieldSql (As a b) where
  sqlType = sqlType @a . coerce

instance Inject (a, [a]) (NonEmpty a) where
  inj (x, xs) = x :| xs

instance Project (a, [a]) (NonEmpty a) where
  prj (x :| xs) = (x, xs)

instance Isomorphic (a, [a]) (NonEmpty a)

deriving via (As (a, [a]) (NonEmpty a)) instance PersistField a => PersistField (NonEmpty a)
deriving via (As (a, [a]) (NonEmpty a)) instance PersistFieldSql a => PersistFieldSql (NonEmpty a)

persist :: String -> [UnboundEntityDef] -> Q [Dec]
persist name =
  share
    [ mkPersist $ sqlSettings
        { mpsFieldLabelModifier = curry snd
        , mpsDeriveInstances = [''Generic, ''Show, ''Eq, ''Ord]
        }
    , mkMigrate ("migrate" ++ name)
    ]
