module Cmd
  ( Cmd
  , runCmd
  , doPrint
  , runIO
  , keyPresses
  , doShutdown
  ) where

import qualified Callbacks as Callback
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

doShutdown' m a = Callback.shutdown (win m) >> return a

doShutdown = wrap doShutdown'

doPrint' s m a = putStrLn s >> return a

doPrint s = wrap $ doPrint' s

runIO' io m a = (: a) <$> io

runIO io = wrap $ runIO' io

keyPresses' keys m a = Keyboard.keyActions (keyMap m) keys >> return a

keyPresses keys = wrap $ keyPresses' keys
