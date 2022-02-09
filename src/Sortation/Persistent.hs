module Sortation.Persistent
  ( module Sortation.Persistent.Collection
  , module Sortation.Persistent.Game
  , module Sortation.Persistent.Release
  , module Sortation.Persistent.Rom
  , module Sortation.Persistent
  )
  where

import Data.Set qualified as Set
import Data.Text (Text)
import Database.Persist.Sql
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

collectionNameFilter :: Text -> Filter Collection
collectionNameFilter collectionName = Filter CollectionName (FilterValue collectionName) Eq

gameFilter :: CollectionId -> Filter Game
gameFilter collectionId = Filter GameParents (FilterValue (Set.singleton collectionId)) In

releaseFilter :: GameId -> Filter Release
releaseFilter gameId = Filter ReleaseParent (FilterValue gameId) Eq

romFilter :: ReleaseId -> Filter Rom
romFilter releaseId = Filter RomParent (FilterValue releaseId) Eq
