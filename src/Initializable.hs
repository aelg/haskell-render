module Initializable
  ( Initializable
  , create
  , destroy
  ) where

class Initializable a where
  create :: IO a
  destroy :: a -> IO ()
