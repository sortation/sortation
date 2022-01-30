module Sortation.Persistent.Quasi where

import Database.Persist.Quasi.Internal
import Database.Persist.TH
import GHC.Generics
import Language.Haskell.TH.Syntax

persist :: String -> [UnboundEntityDef] -> Q [Dec]
persist name =
  share
    [ mkPersist $ sqlSettings
        { mpsFieldLabelModifier = curry snd
        , mpsDeriveInstances = [''Generic, ''Show, ''Eq, ''Ord]
        }
    , mkMigrate ("migrate" ++ name)
    ]
