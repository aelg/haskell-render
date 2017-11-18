module Machine
  ( Machine(..)
  , addAction
  ) where

import           Data.IORef
import           Graphics.Rendering.OpenGL (($=), ($~))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW
import qualified Keyboard                  as Keyboard
import           Shaders

data Machine a = Machine
  { win     :: GLFW.Window
  , actions :: IORef [a]
  , shaders :: Shaders
  , keyMap  :: IORef (Keyboard.KeyActions a)
  }

addAction :: Machine a -> a -> IO ()
addAction m action = actions m $~ (action :)

