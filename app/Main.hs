module Main where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Display
import Callbacks
import Shaders
import Square

main :: IO ()
main = do
  GLFW.init
  GLFW.defaultWindowHints
  Just win <- GLFW.createWindow 640 480 "GLFW" Nothing Nothing
  GLFW.makeContextCurrent (Just win)
  GLFW.setWindowSizeCallback win (Just resizeWindow)
  GLFW.setKeyCallback win (Just keyPressed)
  GLFW.setWindowCloseCallback win (Just shutdown)
  initShaders
  square <- initSquare
  loop win square
  GLFW.destroyWindow win
  GLFW.terminate


