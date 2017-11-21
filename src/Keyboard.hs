module Keyboard
  ( KeyAction(KeyAction, KeyPressed, KeyState)
  , KeyPress(KeyPress)
  , KeyActions
  , keyActions
  , noModifiers
  , keyPressed
  , noActions
  ) where

import           Data.IORef
import qualified Data.Map.Lazy    as M
import           Data.StateVar
import qualified Graphics.UI.GLFW as GLFW

data KeyPress =
  KeyPress GLFW.Key
           GLFW.KeyState
           GLFW.ModifierKeys
  deriving (Ord, Eq)

data KeyAction a
  = KeyAction GLFW.Key
              GLFW.KeyState
              GLFW.ModifierKeys
              (KeyPress -> a)
  | KeyPressed GLFW.Key
               (KeyPress -> a)
  | KeyState GLFW.Key
             GLFW.KeyState
             (KeyPress -> a)

type KeyActions a = M.Map KeyPress (KeyPress -> a)

noActions = M.empty

keyActions :: IORef (KeyActions a) -> [KeyAction a] -> IO ()
keyActions ref actions = do
  let keyMap = M.fromList $ map fromAction actions
  ref $= keyMap
  where
    fromAction (KeyAction a b c d) = (KeyPress a b c, d)
    fromAction (KeyPressed a b) =
      (KeyPress a GLFW.KeyState'Pressed noModifiers, b)
    fromAction (KeyState a b c) = (KeyPress a b noModifiers, c)

noModifiers = GLFW.ModifierKeys False False False False

keyPressed :: (a -> IO ()) -> IORef (KeyActions a) -> GLFW.KeyCallback
keyPressed addAction actions win key _ state modifiers =
  get actions >>= \a ->
    case M.lookup (KeyPress key state modifiers) a of
      Just action -> addAction $ action $ KeyPress key state modifiers
      Nothing     -> return ()
