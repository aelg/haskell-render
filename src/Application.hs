module Application
  ( Application(Application)
  , State
  , Cmd
  , start
  , Config(..)
  ) where

import           CallbackUpdater
import           Cmd
import qualified Control.Monad             as M
import           Data.IORef
import           Graphics.Rendering.OpenGL (($=), ($~))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW
import           Initializable
import qualified Keyboard
import           Machine
import           Shaders
import           State
import qualified Window                    as W

data Application action state = Application
  { update :: action -> state -> State (Cmd action) state
  , view   :: Shaders -> state -> IO ()
  }

data Config =
  DefaultConfig

setupWin DefaultConfig = do
  GLFW.init
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
  GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
  Just win <- GLFW.createWindow 640 480 "GLFW" Nothing Nothing
  GLFW.makeContextCurrent (Just win)
  GL.depthFunc $= Just GL.Less
  return win

setupCallbacks m = do
  wSize <- W.windowSizeCallback (\_ _ -> return ()) (win m)
  wClose <- W.windowCloseCallback (return ()) (win m)
  GLFW.setKeyCallback
    (win m)
    (Just $ Keyboard.keyPressed (addAction m) (keyMap m))
  return $ CallbackUpdater wSize wClose

readActions ::
     Cmd action
  -> Machine action
  -> CallbackUpdater
  -> ([action] -> state)
  -> IO state
readActions cmds m c f = do
  a <- GL.get (actions m)
  actions m $= []
  b <- runCmd cmds (Cmd.Input m c) a
  return $ f b

loop ::
     Machine action
  -> CallbackUpdater
  -> Application action state
  -> State (Cmd action) state
  -> IO ()
loop m c application (State cmds state) = do
  GLFW.pollEvents
  State newCmds newState <-
    readActions cmds m c $ M.foldM (flip (update application)) state
  view application (shaders m) newState
  GLFW.swapBuffers $ win m
  loop m c application $ State newCmds newState

start :: Config -> State (Cmd action) state -> Application action state -> IO ()
start config initial application = do
  win_ <- setupWin config
  actions_ <- newIORef ([] :: [b])
  keyMap_ <- newIORef Keyboard.noActions
  shaders_ <- create
  let m = Machine win_ actions_ shaders_ keyMap_
  callbackUpdater <- setupCallbacks m
  loop m callbackUpdater application initial
  GLFW.destroyWindow win_
  GLFW.terminate
