module Callbacks
  ( resizeWindow
  , shutdown
  ) where

import           Data.IORef
import           Graphics.Rendering.OpenGL as GL
import           Graphics.UI.GLFW          as GLFW
import           System.Exit               (ExitCode (..), exitSuccess)

resizeWindow :: GLFW.WindowSizeCallback
resizeWindow win w h =
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))

shutdown :: GLFW.WindowCloseCallback
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  exitSuccess
