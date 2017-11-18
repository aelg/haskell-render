module Drawable
  ( Drawable
  , draw
  ) where

class Drawable a where
  draw :: a -> IO ()
