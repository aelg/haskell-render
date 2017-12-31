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
  , keyPress
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
  { runCmd :: Input a -> IO [a]
  }

data Input a = Input
  { machine         :: Machine a
  , callbackUpdater :: CallbackUpdater
  }

instance Monoid (Cmd a) where
  mempty = Cmd $ const (return [])
  a `mappend` b = Cmd $ \i -> (++) <$> runCmd a i <*> runCmd b i

newCmd f = State (Cmd f) ()

shutdown :: GLFW.Window -> IO ()
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  exitSuccess

doShutdown = newCmd doShutdown'
  where
    doShutdown' i = shutdown (window i) >> return []
    window = win . machine

doPrint s = newCmd $ doPrint' s
  where
    doPrint' s i = putStrLn s >> return []

runIO :: IO a -> State (Cmd a) ()
runIO io = newCmd $ runIO' io
  where
    runIO' io i = (: []) <$> io

keyPresses :: [Keyboard.KeyAction a] -> State (Cmd a) ()
keyPresses keys = newCmd $ keyPresses' keys
  where
    keyPresses' keys i =
      Keyboard.keyActions (keyMap $ machine i) keys >> return []

getTime :: a -> (Double -> a) -> State (Cmd a) ()
getTime fail success = newCmd $ getTime' fail success
  where
    getTime' fail success i = do
      time <- GLFW.getTime
      return [maybe fail success time]

resize :: (Int -> Int -> a) -> State (Cmd a) ()
resize action = newCmd $ resize' action
  where
    resize' action i =
      updateWindowSizeCallback' i (\w h -> (addAction' i $ action w h)) >>
      return []
    updateWindowSizeCallback' = updateWindowSize . callbackUpdater
    addAction' i = addAction $ machine i

captureMouse :: (Double -> Double -> a) -> State (Cmd a) ()
captureMouse action = newCmd $ captureMouse' action
  where
    captureMouse' action i =
      Mouse.captureMouse
        (win $ machine i)
        (\x y -> (addAction (machine i) $ action x y)) >>
      return []

stopCaptureMouse :: State (Cmd a) ()
stopCaptureMouse = newCmd stopCaptureMouse'
  where
    stopCaptureMouse' i = Mouse.freeMouse (win $ machine i) >> return []

send action = newCmd $ send' action
  where
    send' action i = return [action]

keyPress key pressed notPressed = newCmd $ keyPress' pressed notPressed
  where
    keyPress' pressed notPressed i = do
      press <- getKey (win $ machine i) key
      case press of
        GLFW.KeyState'Pressed -> return [pressed]
        _                     -> return [notPressed]
      where
        press = getKey (win $ machine i) key
