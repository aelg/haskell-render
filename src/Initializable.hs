module Initializable
  ( module Initializable
  ) where

class Initializable a where
  create :: a -> IO a
  destroy :: a -> IO ()
