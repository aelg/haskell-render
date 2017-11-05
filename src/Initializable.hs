module Initializable
  ( module Initializable
  ) where

class Initializable a where
  create :: IO a
  destroy :: a -> IO ()
