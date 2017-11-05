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
  GLFW.defaultWindowHints
  Just win <- GLFW.createWindow 640 480 "GLFW" Nothing Nothing
  GLFW.makeContextCurrent (Just win)
  GLFW.setWindowSizeCallback win (Just resizeWindow)
  GLFW.setKeyCallback win (Just keyPressed)
  GLFW.setWindowCloseCallback win (Just shutdown)
  _ <- create Shaders
  square <- create Square
  loop win [square]
  GLFW.destroyWindow win
  GLFW.terminate
