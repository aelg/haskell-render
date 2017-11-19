module Application
  ( Application(Application)
  , State
  , Cmd
  , start
  , Config(..)
  ) where

import qualified Callbacks                 as Callback
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
  return win

setupCallbacks m = do
  GLFW.setWindowSizeCallback (win m) (Just Callback.resizeWindow)
  GLFW.setKeyCallback
    (win m)
    (Just $ Keyboard.keyPressed (addAction m) (keyMap m))
  GLFW.setWindowCloseCallback (win m) (Just Callback.shutdown)

readActions :: Cmd action -> Machine action -> ([action] -> state) -> IO state
readActions cmds m f = do
  a <- GL.get (actions m)
  actions m $= []
  b <- runCmd cmds m a
  return $ f b

loop ::
     Machine action
  -> Application action state
  -> State (Cmd action) state
  -> IO ()
loop m application (State cmds state) = do
  GLFW.pollEvents
  State newCmds newState <-
    readActions cmds m $ M.foldM (flip (update application)) state
  view application (shaders m) newState
  GLFW.swapBuffers $ win m
  loop m application $ State newCmds newState

start :: Config -> State (Cmd action) state -> Application action state -> IO ()
start config initial application = do
  win_ <- setupWin config
  actions_ <- newIORef ([] :: [b])
  keyMap_ <- newIORef Keyboard.noActions
  shaders_ <- create
  let m = Machine win_ actions_ shaders_ keyMap_
  setupCallbacks m
  loop m application initial
  GLFW.destroyWindow win_
  GLFW.terminate
