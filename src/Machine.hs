module Machine
  ( run
  , Config(DefaultConfig)
  , Application(Application)
  , def
  , spaceBar
  , Action
  ) where

import           Callbacks
import qualified Control.Monad             as M
import           Data.IORef
import           Graphics.Rendering.OpenGL as GL
import           Graphics.UI.GLFW          as GLFW
import           Initializable
import           Shaders
import           Square

data (Action a, Show a) =>
     Cmd a
  = SpaceBar a
  | NoCmd
  deriving (Show)

data Action b =>
     State b a =
  State ([Cmd b])
        a

instance Action b => Functor (State b) where
  fmap f (State b a) = State b $ f a

instance Action b => Applicative (State b) where
  State b f <*> State c a = State c $ f a
  pure = State [NoCmd]

instance Action b => Monad (State b) where
  State b a >>= f =
    let State c d = f a
    in State (b ++ c) d
  return = pure

class Action a where
  def :: a

data Action b =>
     Application a b = Application
  { update :: a -> b -> State b a
  , view   :: Shaders -> a -> IO ()
  }

data Config =
  DefaultConfig

data Machine =
  Machine

data NoAction =
  NoAction

instance Action NoAction where
  def = NoAction

data Action a =>
     Internal a =
  Internal a

setupWin DefaultConfig = do
  GLFW.init
  GLFW.windowHint $ WindowHint'ContextVersionMajor 3
  GLFW.windowHint $ WindowHint'ContextVersionMinor 3
  GLFW.windowHint $ WindowHint'OpenGLProfile OpenGLProfile'Core
  Just win <- GLFW.createWindow 640 480 "GLFW" Nothing Nothing
  GLFW.makeContextCurrent (Just win)
  return win

setupCallbacks win = do
  GLFW.setWindowSizeCallback win (Just resizeWindow)
  GLFW.setKeyCallback win (Just keyPressed)
  GLFW.setWindowCloseCallback win (Just shutdown)

spaceBar :: (Show a, Action a) => a -> State a b -> State a b
spaceBar action (State _ state) = State [SpaceBar action] state

loop ::
     (Show b, Action b)
  => GLFW.Window
  -> Shaders
  -> Application a b
  -> State b a
  -> IO ()
loop win shaders application (State _ state) = do
  let State cmd1 state1 = update application state def
      State cmd2 state2 =
        case cmd1 of
          [SpaceBar action] -> update application state action
          otherwise         -> State cmd1 state1
  print cmd1
  print cmd2
  -- Todo handle cmd
  GLFW.pollEvents
  view application shaders state2
  GLFW.swapBuffers win
  loop win shaders application $ State [NoCmd] state2

run :: (Action b, Show b) => Config -> IO a -> Application a b -> IO ()
run config initial application = do
  win <- setupWin config
  setupCallbacks win
  shaders <- create
  state <- initial
  loop win shaders application $ State [NoCmd] state
  GLFW.destroyWindow win
  GLFW.terminate
