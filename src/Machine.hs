module Machine
  ( run
  , Config(DefaultConfig)
  , Application(Application)
  , doShutdown
  , redraw
  , doPrint
  , runIO
  , keyPresses
  , State.State
  ) where

import           Application
import qualified Callbacks                 as Callback
import           Cmd
import qualified Control.Monad             as M
import           Data.IORef
import           Graphics.Rendering.OpenGL as GL
import           Graphics.UI.GLFW          as GLFW
import           Initializable
import qualified Keyboard                  as Keyboard
import           Shaders
import           Square
import           State

data Config =
  DefaultConfig

data Machine a = Machine
  { win     :: GLFW.Window
  , actions :: IORef [a]
  , shaders :: Shaders
  , keyMap  :: IORef (Keyboard.KeyActions a)
  }

setupWin DefaultConfig = do
  GLFW.init
  GLFW.windowHint $ WindowHint'ContextVersionMajor 3
  GLFW.windowHint $ WindowHint'ContextVersionMinor 3
  GLFW.windowHint $ WindowHint'OpenGLProfile OpenGLProfile'Core
  Just win <- GLFW.createWindow 640 480 "GLFW" Nothing Nothing
  GLFW.makeContextCurrent (Just win)
  return win

setupCallbacks m = do
  GLFW.setWindowSizeCallback (win m) (Just Callback.resizeWindow)
  GLFW.setKeyCallback
    (win m)
    (Just $ Keyboard.keyPressed (addAction m) (keyMap m))
  GLFW.setWindowCloseCallback (win m) (Just Callback.shutdown)

addCmd :: Cmd b -> State b ()
addCmd c = State [c] ()

doShutdown :: State b ()
doShutdown = addCmd Shutdown

redraw :: State b ()
redraw = addCmd Redraw

doPrint :: String -> State b ()
doPrint s = addCmd $ Print s

runIO :: IO b -> State b ()
runIO a = addCmd $ RunIO a

keyPresses :: [(Keyboard.KeyAction, b)] -> State b ()
keyPresses actions = addCmd $ KeyPress actions

addAction :: Machine a -> a -> IO ()
addAction m action = actions m $~ (action :)

handleCmd :: Machine a -> Cmd a -> IO ()
handleCmd _ Redraw             = return () -- --------!!!!!
handleCmd m Shutdown           = Callback.shutdown (win m)
handleCmd _ (Print s)          = putStrLn s
handleCmd m (KeyPress actions) = Keyboard.keyActions (keyMap m) actions
handleCmd m (RunIO a)          = a >>= \action -> addAction m action

readActions :: Machine a -> ([a] -> b) -> IO b
readActions m f = do
  a <- get (actions m)
  actions m $= []
  return (f a)

loop :: Machine b -> Application a b -> State b a -> IO ()
loop m application (State cmds state) = do
  mapM_ (handleCmd m) (cmds)
  GLFW.pollEvents

  State newCmds newState <- readActions m $ M.foldM (update application) state

  view application (shaders m) newState
  GLFW.swapBuffers $ win m
  loop m application $ State newCmds newState

run :: Config -> State b a -> Application a b -> IO ()
run config initial application = do
  win_ <- setupWin config
  actions_ <- newIORef ([] :: [b])
  keyMap_ <- newIORef Keyboard.empty
  shaders_ <- create
  let m = Machine win_ actions_ shaders_ keyMap_
  setupCallbacks m
  loop m application initial
  GLFW.destroyWindow win_
  GLFW.terminate
