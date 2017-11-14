module Callbacks
  ( resizeWindow
  , keyPressed
  , shutdown
  , keyMap
  , initialKeyMap
  , noModifiers
  ) where

import           Graphics.Rendering.OpenGL as GL
import           Graphics.UI.GLFW          as GLFW
import           System.Exit               (ExitCode (..), exitSuccess)
import qualified Data.Map.Lazy             as M
import           Data.IORef
import           InternalState

resizeWindow :: GLFW.WindowSizeCallback
resizeWindow win w h =
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))

type KeyAction = (GLFW.Key, GLFW.KeyState, GLFW.ModifierKeys)
type KeyActionMap = M.Map KeyAction

keyMap :: KeyAction -> a ->  KeyActionMap a -> KeyActionMap a
keyMap = M.insert

initialKeyMap :: KeyActionMap a
initialKeyMap = M.empty

noModifiers = GLFW.ModifierKeys False False False False

keyPressed :: IORef [a] ->  KeyActionMap a -> GLFW.KeyCallback
keyPressed ref m win key _ state modifiers = case M.lookup (key, state, modifiers) m of
  Just a -> ref $~ (a:)
  Nothing -> return ()

shutdown :: GLFW.WindowCloseCallback
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  exitSuccess
