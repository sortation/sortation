module Data.CasMap
  ( CasMap
  , CasKey
  , empty
  , insert
  , insertAll
  , index
  , indexAll
  )
  where

import Data.Foldable
import Data.Hashable
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind
import Data.Serialize
import GHC.Exts
import GHC.Generics

type CasMap :: forall k. k -> Type -> Type
newtype CasMap l a = CasMap (IntMap a)
  deriving newtype (Hashable, Serialize, Semigroup, Monoid, Eq, Ord, Show, Read)

instance Functor (CasMap l) where
  fmap f (CasMap m) = CasMap (IntMap.map f m)

instance Foldable (CasMap l) where
  foldr f x (CasMap m) = IntMap.foldr' f x m
  foldl f x (CasMap m) = IntMap.foldl' f x m

type CasKey :: forall k. k -> Type -> Type
newtype CasKey l a = CasKey Int
  deriving stock (Generic, Eq, Ord, Show, Read)
  deriving anyclass (Hashable, Serialize)

empty :: forall l a. CasMap l a
empty = CasMap IntMap.empty

insert :: forall l a. Hashable a => CasMap l a -> a -> (CasKey l a, CasMap l a)
insert (CasMap m) x = let h = hash x in (CasKey h, CasMap (IntMap.insert h x m))

insertAll :: forall l a t. (Hashable a, Foldable t) => CasMap l a -> t a -> ([CasKey l a], CasMap l a)
insertAll m =
  foldl'
    (\(keys, map) x -> let (key, map') = insert map x in (key:keys, map'))
    ([], empty)

index :: forall l a. CasMap l a -> CasKey l a -> a
index (CasMap m) (CasKey i) = m IntMap.! i

indexAll :: forall l a f. Functor f => CasMap l a -> f (CasKey l a) -> f a
indexAll map = fmap (index map)
