module Sortation.Persistent
  ( module Sortation.Persistent.Collection
  , module Sortation.Persistent.Game
  , module Sortation.Persistent.Release
  , module Sortation.Persistent.Rom
  , migrateAll
  )
  where

import Database.Persist.Sql.Migration
import Sortation.Persistent.Collection
import Sortation.Persistent.Game
import Sortation.Persistent.Release
import Sortation.Persistent.Rom

migrateAll :: Migration
migrateAll = sequence_ @[]
  [ migrateRom
  , migrateRelease
  , migrateGame
  , migrateCollection
  ]
