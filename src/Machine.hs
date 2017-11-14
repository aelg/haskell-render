module Machine
  ( run
  , Config(DefaultConfig)
  , Application(Application)
  , spaceBar
  , doShutdown
  , redraw
  , doPrint
  , runIO
  ) where

import           Application
import           Callbacks
import           Cmd
import qualified Control.Monad             as M
import           Data.IORef
import           Graphics.Rendering.OpenGL as GL
import           Graphics.UI.GLFW          as GLFW
import           Initializable
import           InternalState
import           Shaders
import           Square
import           State

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
  GLFW.setKeyCallback
    (win iState)
    (Just
       (keyPressed
          (actions iState)
          (keyMap escapePressed shutdownAction initialKeyMap)))
  GLFW.setWindowCloseCallback (win iState) (Just shutdown)

spaceBar :: b -> a -> State b a
spaceBar action = State [SpaceBar action]

doShutdown :: State b a -> State b a
doShutdown (State _ state) = State [Shutdown] state

redraw :: a -> State b a
redraw = State [Redraw]

doPrint :: String -> a -> State b a
doPrint s = State [Print s]

runIO :: IO b -> a -> State b a
runIO f = State [RunIO f]

handleCmd :: InternalState a -> Cmd a -> IO ()
handleCmd iState NoCmd             = return ()
handleCmd iState Redraw            = return () -- --------!!!!!
handleCmd iState Shutdown          = shutdown (win iState)
handleCmd iState (Print s)         = putStrLn s
handleCmd iState (SpaceBar action) = actions iState $~ (action :)
handleCmd iState (RunIO a)         = a >>= \action -> actions iState $~ (action:)

loop :: InternalState b -> Application a b -> State b a -> IO ()
loop iState application (State cmds state) = do
  GLFW.pollEvents
  currentActions <- get (actions iState)
  actions iState $= ([] :: [a])
  State newCmds newState <-
    return $ M.foldM (update application) state currentActions
  mapM_ (handleCmd iState) (cmds ++ newCmds)
  view application (shaders iState) newState
  GLFW.swapBuffers $ win iState
  loop iState application $ State [NoCmd] newState

run :: Config -> State b a -> b -> Application a b -> IO ()
run config initial shutdownAction application = do
  win_ <- setupWin config
  actions_ <- newIORef ([] :: [b])
  shaders_ <- create
  let iState = InternalState win_ actions_ shaders_
  setupCallbacks iState shutdownAction
  loop iState application initial
  GLFW.destroyWindow win_
  GLFW.terminate
