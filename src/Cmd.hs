module Cmd
  ( Cmd
  , runCmd
  , doPrint
  , runIO
  , keyPresses
  , doShutdown
  ) where

import           Keyboard
import State
import Machine
import qualified Callbacks as Callback

data Cmd a = Cmd { runCmd :: Machine a -> [a] -> IO [a] }

instance Monoid (Cmd a) where
  mempty = Cmd $ \m actions -> return actions
  mappend a b = Cmd $ \m actions -> runCmd a m actions >>= runCmd b m

wrap f = State (Cmd (f)) ()

doShutdown_ m a = Callback.shutdown (win m) >> return a
doShutdown = wrap doShutdown_

doPrint_ s m a = putStrLn s >> return a
doPrint s = wrap $ doPrint_ s

runIO_ io m a = (:a) <$> io
runIO io = wrap $ runIO_ io

keyPresses_ keys m a = Keyboard.keyActions (keyMap m) keys >> return a
keyPresses keys = wrap $ keyPresses_ keys
