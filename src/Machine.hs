module Machine
  ( run
  , Config(DefaultConfig)
  , Application(Application)
  , spaceBar
  , doShutdown
  , redraw
  , doPrint
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
import           InternalState

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

setupCallbacks iState shutdownAction = do
  GLFW.setWindowSizeCallback (win iState) (Just resizeWindow)
  GLFW.setKeyCallback (win iState) 
    (Just (keyPressed (actions iState) (keyMap escapePressed shutdownAction initialKeyMap)))
  GLFW.setWindowCloseCallback (win iState) (Just shutdown)

spaceBar :: (Show b) => b -> State b a -> State b a
spaceBar action (State _ state) = State [SpaceBar action] state

doShutdown :: State b a -> State b a
doShutdown (State _ state) = State [Shutdown] state

redraw :: a -> State b a
redraw = State [Redraw]

doPrint :: String -> a -> State b a
doPrint s = State [Print s]

handleCmd :: InternalState a -> Cmd a -> IO ()
handleCmd iState Shutdown = shutdown (win iState)
handleCmd iState (Print s) = putStrLn s
handleCmd iState (SpaceBar action) = actions iState $~ (action:)

loop ::
     (Show b)
  => InternalState b
  -> Application a b
  -> State b a
  -> IO ()
loop iState application (State _ state) = do
  GLFW.pollEvents
  currentActions <- get (actions iState)
  actions iState $= ([] :: [a])
  State cmds newState <- return $ M.foldM (update application) state currentActions
  mapM_ (handleCmd iState) cmds
  print currentActions
  (view application) (shaders iState) newState
  GLFW.swapBuffers $ win iState
  loop iState application $ State [NoCmd] newState

run :: (Show b) => Config -> State b a -> b -> Application a b -> IO ()
run config initial shutdownAction application = do
  win_ <- setupWin config
  actions_ <- newIORef ([] :: [b])
  shaders_ <- create
  let iState = InternalState win_ actions_ shaders_
  setupCallbacks iState shutdownAction
  loop iState application initial
  GLFW.destroyWindow win_
  GLFW.terminate
