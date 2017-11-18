module Cmd
  ( Cmd
  , redraw
  , doPrint
  , runIO
  , keyPresses
  , doShutdown
  , handleCmd
  ) where

import           Keyboard
import State
import Machine
import qualified Callbacks as Callback

data Cmd a
  = KeyPress [(KeyAction, a)]
  | Shutdown
  | Redraw
  | Print String
  | RunIO (IO a)
  | GetTime (Maybe Double -> a)

addCmd :: Cmd b -> State [Cmd b] ()
addCmd c = State [c] ()

doShutdown :: State [Cmd a] ()
doShutdown = addCmd Shutdown

redraw :: State [Cmd a] ()
redraw = addCmd Redraw

doPrint :: String -> State [Cmd a] ()
doPrint s = addCmd $ Print s

runIO :: IO a -> State [Cmd a] ()
runIO a = addCmd $ RunIO a

keyPresses :: [(Keyboard.KeyAction, a)] -> State [Cmd a] ()
keyPresses actions = addCmd $ KeyPress actions

getTime :: (Double -> a) -> a -> State [Cmd a] ()
getTime success fail = addCmd $ GetTime f

handleCmd :: Machine a -> Cmd a -> IO ()
handleCmd _ Redraw             = return () -- --------!!!!!
handleCmd m Shutdown           = Callback.shutdown (win m)
handleCmd _ (Print s)          = putStrLn s
handleCmd m (KeyPress actions) = Keyboard.keyActions (keyMap m) actions
handleCmd m (RunIO a)          = a >>= \action -> addAction m action

