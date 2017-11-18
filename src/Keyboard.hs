module Keyboard
  ( KeyAction(KeyAction)
  , KeyActions
  , keyActions
  , noModifiers
  , keyPressed
  , empty
  ) where

import           Data.IORef
import qualified Data.Map.Lazy    as M
import           Data.StateVar
import qualified Graphics.UI.GLFW as GLFW

data KeyAction =
  KeyAction GLFW.Key
            GLFW.KeyState
            GLFW.ModifierKeys
  deriving (Ord, Eq)

type KeyActions = M.Map KeyAction

empty = M.empty

keyActions :: IORef (KeyActions a) -> [(KeyAction, a)] -> IO ()
keyActions ref actions = do
  let map = M.fromList actions
  ref $= map

noModifiers = GLFW.ModifierKeys False False False False

keyPressed :: (a -> IO ()) -> IORef (KeyActions a) -> GLFW.KeyCallback
keyPressed addAction actions win key _ state modifiers =
  get actions >>= \a ->
    case M.lookup (KeyAction key state modifiers) a of
      Just action -> addAction action
      Nothing     -> return ()
