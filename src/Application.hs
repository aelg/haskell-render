module Application (Application(..)) where 

import State
import Shaders

data Application a b = Application
  { update :: a -> b -> State b a
  , view   :: Shaders -> a -> IO ()
  , frameAction :: b
  }
