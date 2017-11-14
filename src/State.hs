module State
  ( State(..)
  ) where

import           Cmd

data State b a =
  State [Cmd b]
        a

instance Functor (State b) where
  fmap f (State b a) = State b $ f a

instance Applicative (State b) where
  State b f <*> State c a = State (b ++ c) $ f a
  pure = State []

instance Monad (State b) where
  State b a >>= f =
    let State c d = f a
    in State (b ++ c) d
  return = pure
