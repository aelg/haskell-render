module Cmd
  ( Cmd
  , Input(..)
  , runCmd
  , doPrint
  , runIO
  , keyPresses
  , captureMouse
  , stopCaptureMouse
  , resize
  , doShutdown
  , getTime
  , send
  ) where

import           CallbackUpdater
import qualified Graphics.UI.GLFW as GLFW
import           Keyboard
import           Machine
import qualified Mouse
import           State
import           System.Exit      (ExitCode (..), exitSuccess)
import qualified Window           as W

newtype Cmd a = Cmd
  { runCmd :: Input a -> [a] -> IO [a]
  }

data Input a = Input
  { machine         :: Machine a
  , callbackUpdater :: CallbackUpdater
  }

instance Monoid (Cmd a) where
  mempty = Cmd $ \m actions -> return actions
  mappend a b = Cmd $ \i actions -> runCmd a i actions >>= runCmd b i

wrap f = State (Cmd f) ()

append f a = fmap (: a) f

shutdown :: GLFW.Window -> IO ()
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  exitSuccess

doShutdown = wrap doShutdown'
  where
    doShutdown' i a = shutdown (window i) >> return a
    window = win . machine

doPrint s = wrap $ doPrint' s
  where
    doPrint' s i a = putStrLn s >> return a

runIO io = wrap $ runIO' io
  where
    runIO' io i a = io `append` a

keyPresses :: [Keyboard.KeyAction a] -> State (Cmd a) ()
keyPresses keys = wrap $ keyPresses' keys
  where
    keyPresses' keys i a =
      Keyboard.keyActions (keyMap $ machine i) keys >> return a

getTime fail success = wrap $ getTime' fail success
  where
    getTime' fail success i a = do
      time <- GLFW.getTime
      return (maybe fail success time) `append` a

resize :: (Int -> Int -> a) -> State (Cmd a) ()
resize action = wrap $ resize' action
  where
    resize' action i a =
      updateWindowSizeCallback' i (\w h -> (addAction' i $ action w h)) >>
      return a
    updateWindowSizeCallback' = updateWindowSize . callbackUpdater
    addAction' i = addAction $ machine i

captureMouse :: (Double -> Double -> a) -> State (Cmd a) ()
captureMouse action = wrap $ captureMouse' action
  where
    captureMouse' action i a =
      Mouse.captureMouse
        (win $ machine i)
        (\x y -> (addAction (machine i) $ action x y)) >>
      return a

stopCaptureMouse :: State (Cmd a) ()
stopCaptureMouse = wrap stopCaptureMouse'
  where
    stopCaptureMouse' i a = Mouse.freeMouse (win $ machine i) >> return a

send action = wrap $ send' action
  where
    send' action i a = return action `append` a
