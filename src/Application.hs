module Application
  ( Application(Application)
  , State
  , Cmd
  , run
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

data Application a b = Application
  { update :: a -> b -> State (Cmd b) a
  , view   :: Shaders -> a -> IO ()
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

readActions :: Cmd a -> Machine a -> ([a] -> b) -> IO b
readActions cmds m f = do
  a <- GL.get (actions m)
  actions m $= []
  b <- runCmd cmds m a
  return $ f b

loop :: Machine b -> Application a b -> State (Cmd b) a -> IO ()
loop m application (State cmds state) = do
  GLFW.pollEvents
  State newCmds newState <-
    readActions cmds m $ M.foldM (update application) state
  view application (shaders m) newState
  GLFW.swapBuffers $ win m
  loop m application $ State newCmds newState

run :: Config -> State (Cmd b) a -> Application a b -> IO ()
run config initial application = do
  win_ <- setupWin config
  actions_ <- newIORef ([] :: [b])
  keyMap_ <- newIORef Keyboard.noActions
  shaders_ <- create
  let m = Machine win_ actions_ shaders_ keyMap_
  setupCallbacks m
  loop m application initial
  GLFW.destroyWindow win_
  GLFW.terminate
