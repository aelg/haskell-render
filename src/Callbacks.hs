module Callbacks (resizeWindow, keyPressed, shutdown) where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import System.Exit (exitSuccess, ExitCode(..))

resizeWindow :: GLFW.WindowSizeCallback
resizeWindow win w h = do
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GL.ortho2D 0 (realToFrac w) (realToFrac h) 0

keyPressed :: GLFW.KeyCallback
keyPressed win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = shutdown win
keyPressed _ _ _ _ _ = return ()

shutdown :: GLFW.WindowCloseCallback
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  exitSuccess
