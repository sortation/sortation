module Effectful.Consume where

import Data.Maybe
import Effectful.Dispatch.Static

data Consume :: Type -> Effect

type instance DispatchOf (Consume a) = Static NoSideEffects
newtype instance StaticRep (Consume a) = Consume { stack :: [a] }
  deriving Generic

runConsume :: Foldable f => f a -> Eff (Consume a : es) ~> Eff es
runConsume stack = evalStaticRep (Consume (toList stack))

consume :: forall a es. Consume a :> es => Eff es (Maybe a)
consume =
  uncons . (.stack) <$> getStaticRep @(Consume a) >>= \case
    Nothing -> pure Nothing
    Just (head, tail) -> do
      putStaticRep $ Consume tail
      pure $ Just head

peek :: forall a es. Consume a :> es => Eff es (Maybe a)
peek =
  uncons . (.stack) <$> getStaticRep @(Consume a) >>= \case
    Nothing -> pure Nothing
    Just (head, _) -> pure $ Just head

consumeWhile :: forall a es. Consume a :> es => (a -> Bool) -> Eff es [a]
consumeWhile p = whileM (maybe False p <$> peek) (fromJust <$> consume)
