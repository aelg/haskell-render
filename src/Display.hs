module Display (loop) where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Control.Monad
import qualified Foreign.Ptr as Ptr

import Descriptor
import Square
import Drawable


onDisplay :: Drawable a => Window -> [a] -> IO ()
onDisplay win primitives = do
  GL.clearColor $= Color4 1 0 0 1
  GL.clear [ColorBuffer]
  mapM_ draw primitives
  GLFW.swapBuffers win


loop :: Drawable a => Window -> [a] -> IO ()
loop win primitives = 
  forever $ do
    GLFW.pollEvents
    onDisplay win primitives

