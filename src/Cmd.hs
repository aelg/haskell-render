module Cmd
  ( Cmd
  , runCmd
  , doPrint
  , runIO
  , keyPresses
  , doShutdown
  , getTime
  , send
  ) where

import qualified Callbacks        as Callback
import qualified Graphics.UI.GLFW as GLFW
import           Keyboard
import           Machine
import           State

newtype Cmd a = Cmd
  { runCmd :: Machine a -> [a] -> IO [a]
  }

instance Monoid (Cmd a) where
  mempty = Cmd $ \m actions -> return actions
  mappend a b = Cmd $ \m actions -> runCmd a m actions >>= runCmd b m

wrap f = State (Cmd f) ()

append f a = fmap (: a) f

doShutdown' m a = Callback.shutdown (win m) >> return a

doShutdown = wrap doShutdown'

doPrint' s m a = putStrLn s >> return a

doPrint s = wrap $ doPrint' s

runIO' io m a = io `append` a

runIO io = wrap $ runIO' io

keyPresses' keys m a = Keyboard.keyActions (keyMap m) keys >> return a

keyPresses keys = wrap $ keyPresses' keys

getTime' success fail m a = do
  time <- GLFW.getTime
  return (maybe fail success time) `append` a

getTime success fail = wrap $ getTime' success fail

send' action m a = return action `append` a

send action = wrap $ send' action
