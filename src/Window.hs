module Window
  ( WindowSizeCallback
  , windowSizeCallback
  , WindowCloseCallback
  , windowCloseCallback
  ) where

import           Control.Monad
import           Data.IORef
import           Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW

type WindowSizeCallback = Int -> Int -> IO ()

type WindowCloseCallback = IO ()

resizeWindow :: IORef WindowSizeCallback -> GLFW.WindowSizeCallback
resizeWindow ref win w h = do
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
  f <- readIORef ref
  f w h

windowSizeCallback ::
     WindowSizeCallback -> GLFW.Window -> IO (WindowSizeCallback -> IO ())
windowSizeCallback initialCallback window = do
  ref <- newIORef initialCallback
  GLFW.setWindowSizeCallback window (Just $ resizeWindow ref)
  return $ \f -> writeIORef ref f

windowClose :: IORef WindowCloseCallback -> GLFW.WindowCloseCallback
windowClose ref win = join $ readIORef ref

windowCloseCallback ::
     WindowCloseCallback -> GLFW.Window -> IO (WindowCloseCallback -> IO ())
windowCloseCallback initialCallback window = do
  ref <- newIORef initialCallback
  GLFW.setWindowCloseCallback window (Just $ windowClose ref)
  return $ \f -> writeIORef ref f
