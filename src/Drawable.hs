module Drawable
  ( module Drawable
  ) where

class Drawable a where
  draw :: a -> IO ()
