module InternalState
  ( InternalState(..)
  ) where

import           Data.IORef
import qualified Graphics.UI.GLFW as GLFW
import           Shaders

data InternalState a = InternalState
  { win     :: GLFW.Window
  , actions :: IORef [a]
  , shaders :: Shaders
  }
