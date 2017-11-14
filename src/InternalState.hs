module InternalState (InternalState(..)) where

import qualified          Graphics.UI.GLFW          as GLFW
import Shaders
import Data.IORef

data InternalState a = InternalState {win :: GLFW.Window
 , actions :: IORef [a]
 , shaders :: Shaders}
