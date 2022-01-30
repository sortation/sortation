module Sortation.Persistent.Collection where

import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH
import Data.Set (Set)
import Data.Text (Text)
import Sortation.Persistent.Quasi

persist "Collection" [persistLowerCase|
  Collection
    name Text
    parents (Set CollectionId)
|]
