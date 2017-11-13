module Machine
  ( run
  , Config(DefaultConfig)
  , Application(Application)
  , spaceBar
  , doShutdown
  ) where

import qualified Control.Monad             as M
import           Data.IORef
import           Graphics.Rendering.OpenGL as GL
import           Graphics.UI.GLFW          as GLFW
import           Initializable
import           Callbacks
import           Shaders
import           Square
import           Application
import           State
import           Cmd

data Config =
  DefaultConfig

data Machine =
  Machine

setupWin DefaultConfig = do
  GLFW.init
  GLFW.windowHint $ WindowHint'ContextVersionMajor 3
  GLFW.windowHint $ WindowHint'ContextVersionMinor 3
  GLFW.windowHint $ WindowHint'OpenGLProfile OpenGLProfile'Core
  Just win <- GLFW.createWindow 640 480 "GLFW" Nothing Nothing
  GLFW.makeContextCurrent (Just win)
  return win

escapePressed = (GLFW.Key'Escape, GLFW.KeyState'Pressed, noModifiers)

setupCallbacks actions shutdownAction win = do
  GLFW.setWindowSizeCallback win (Just resizeWindow)
  GLFW.setKeyCallback win (Just (keyPressed actions (keyMap escapePressed shutdownAction initialKeyMap)))
  GLFW.setWindowCloseCallback win (Just shutdown)

spaceBar :: (Show b) => b -> State b a -> State b a
spaceBar action (State _ state) = State [SpaceBar action] state

doShutdown :: State b a -> State b a
doShutdown (State _ state) = State [Shutdown] state

loop ::
     (Show b)
  => IORef [b]
  -> GLFW.Window
  -> Shaders
  -> Application a b
  -> State b a
  -> IO ()
loop actions win shaders application (State _ state) = do
  let State cmd1 state1 = update application state (frameAction application)
      State cmd2 state2 =
        case cmd1 of
          [SpaceBar action] -> update application state action
          otherwise         -> State cmd1 state1
  print cmd1
  print cmd2
  -- Todo handle cmd
  GLFW.pollEvents
  currentActions <- get actions
  actions $= ([] :: [a])
  State cmd3 state3 <- return $ M.foldM (update application) state2 currentActions
  case cmd3 of
     [Shutdown] -> shutdown win
     otherwise  -> return ()
  print currentActions
  view application shaders state3
  GLFW.swapBuffers win
  loop actions win shaders application $ State [NoCmd] state2

run :: (Show b) => Config -> IO a -> b -> Application a b -> IO ()
run config initial shutdownAction application = do
  win <- setupWin config
  actions <- newIORef ([] :: [b])
  setupCallbacks actions shutdownAction win
  shaders <- create
  state <- initial
  loop actions win shaders application $ State [NoCmd] state
  GLFW.destroyWindow win
  GLFW.terminate
