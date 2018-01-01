module Mouse
  ( captureMouse
  , freeMouse
  ) where

import qualified Graphics.UI.GLFW as GLFW

cursorPosCallback :: (Double -> Double -> IO ()) -> GLFW.CursorPosCallback
cursorPosCallback f win x y = do
  (width, height) <- GLFW.getWindowSize win
  let x0 = fromIntegral width / 2
      y0 = fromIntegral height / 2
  GLFW.setCursorPos win x0 y0
  f (x - x0) (y - y0)

captureMouse :: GLFW.Window -> (Double -> Double -> IO ()) -> IO ()
captureMouse win f = do
  GLFW.setCursorInputMode win GLFW.CursorInputMode'Disabled
  GLFW.setCursorPos win 0 0
  GLFW.setCursorPosCallback win (Just (cursorPosCallback f))

freeMouse win = do
  GLFW.setCursorInputMode win GLFW.CursorInputMode'Normal
  GLFW.setCursorPosCallback win Nothing
