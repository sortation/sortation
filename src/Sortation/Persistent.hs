module Sortation.Persistent
  ( module Sortation.Persistent.File
  , module Sortation.Persistent.Rom
  , module Sortation.Persistent.RomSet
  , module Sortation.Persistent
  )
  where

import Data.Set qualified as Set
import Data.Text (Text)
import Database.Persist.Sql
import Database.Persist.Sql.Migration
import Database.Persist.TH
import Sortation.Persistent.File
import Sortation.Persistent.Quasi
import Sortation.Persistent.Rom
import Sortation.Persistent.RomSet

migrateAll :: Migration
migrateAll = do
  migrateFile
  migrateRom
  migrateRomFile
  migrateRomSet
  migrateRomSetRom

-- collectionNameFilter :: Text -> Filter Collection
-- collectionNameFilter collectionName = Filter CollectionName (FilterValue collectionName) Eq
-- 
-- gameFilter :: CollectionId -> Filter Game
-- gameFilter collectionId = Filter GameParents (FilterValue (Set.singleton collectionId)) In
-- 
-- releaseFilter :: GameId -> Filter Release
-- releaseFilter gameId = Filter ReleaseParent (FilterValue gameId) Eq
-- 
-- romFilter :: ReleaseId -> Filter Rom
-- romFilter releaseId = Filter RomParent (FilterValue releaseId) Eq
