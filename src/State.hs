module State
  ( State(..)
  ) where

data State b a =
  State b
        a

instance Monoid b => Functor (State b) where
  fmap f (State b a) = State b $ f a

instance Monoid b => Applicative (State b) where
  State b f <*> State c a = State (b `mappend` c) $ f a
  pure = State mempty

instance Monoid b => Monad (State b) where
  State b a >>= f =
    let State c d = f a
    in State (b `mappend` c) d
  return = pure
