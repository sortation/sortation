module Sortation.Persistent.Game where

import Data.Set (Set)
import Data.Text (Text)
import Database.Persist
import Database.Persist.TH
import Sortation.Persistent.Collection (CollectionId)
import Sortation.Persistent.Quasi

persist "Game" [persistLowerCase|
  Game
    name Text
    parents (Set CollectionId)
|]
