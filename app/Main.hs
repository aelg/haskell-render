module Main
  ( module Main
  ) where

import           Callbacks
import           Display
import           Graphics.Rendering.OpenGL as GL
import           Graphics.UI.GLFW          as GLFW
import           Initializable
import           Shaders
import           Square

main :: IO ()
main = do
  GLFW.init
  GLFW.windowHint $ WindowHint'ContextVersionMajor 3
  GLFW.windowHint $ WindowHint'ContextVersionMinor 3
  GLFW.windowHint $ WindowHint'OpenGLProfile OpenGLProfile'Core
  Just win <- GLFW.createWindow 640 480 "GLFW" Nothing Nothing
  GLFW.makeContextCurrent (Just win)
  GLFW.setWindowSizeCallback win (Just resizeWindow)
  GLFW.setKeyCallback win (Just keyPressed)
  GLFW.setWindowCloseCallback win (Just shutdown)
  shaders <- create
  square <- create :: IO Square
  loop win shaders [square]
  GLFW.destroyWindow win
  GLFW.terminate
