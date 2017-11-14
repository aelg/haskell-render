module Application
  ( Application(..)
  ) where

import           Shaders
import           State

data Application a b = Application
  { update      :: a -> b -> State b a
  , view        :: Shaders -> a -> IO ()
  , frameAction :: b
  }
