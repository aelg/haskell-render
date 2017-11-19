module Main
  ( main
  ) where

import           Control.Monad
import           Data.Vec                  ((:.))
import qualified Data.Vec                  as Vec
import           Foreign.Marshal
import qualified Foreign.Ptr               as Ptr
import qualified Foreign.Storable          as S
import qualified Graphics.GL.Functions     as GLF
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW

import           Application
import           Cmd
import           Drawable
import           Initializable
import           Keyboard
import           Shaders
import           Square

--State
data Color
  = Red
  | Green
  | Blue
  deriving (Show)

getColor Red   = GL.Color3 1.0 0.0 (0.0 :: GL.GLfloat)
getColor Green = GL.Color3 0.0 1.0 (0.0 :: GL.GLfloat)
getColor Blue  = GL.Color3 0.0 0.0 (1.0 :: GL.GLfloat)

data MyState
  = Empty
  | MyState { primitives :: [Square]
            , color      :: Color
            , lastSecond :: Double }
  deriving (Show)

--Actions
--data MyAction
--  = Frame
--  | SwapColor
--  | Shutdown
--  | Spacebar
--  | Initial MyState
--  | Time Double
--  | TimeFail
--  deriving (Show)
newtype Action = Action
  { runAction :: MyState -> State (Cmd Action) MyState
  }

type Update a = State (Cmd Action) a

-- Update
blueGreen Blue  = Green
blueGreen Green = Blue

run :: (MyState -> Update MyState) -> Action
run f = Action $ \state -> f state

run1 :: (a -> MyState -> Update MyState) -> a -> Action
run1 f a = Action $ \state -> f a state

shutdown :: MyState -> Update MyState
shutdown state = do
  doPrint "Will shutdown"
  doShutdown
  return state

swapColor :: MyState -> Update MyState
swapColor (state@MyState {color = c}) = return $ state {color = blueGreen c}

askTime :: Update ()
askTime = getTime (run timeFail) (run1 gotTime)

gotTime :: Double -> MyState -> Update MyState
gotTime a (state@MyState {lastSecond = s}) = do
  askTime
  if a > s
    then do
      send $ run swapColor
      return $ state {lastSecond = s + 1}
    else return state

timeFail :: MyState -> Update MyState
timeFail state = do
  askTime
  doPrint "Time: failed"
  return state

--View
setColor program color = do
  colorUniform <- GL.uniformLocation program "color"
  GL.uniform colorUniform $= getColor color

--vec3 :: forall a a1 a2. a -> a1 -> a2 -> a :. (a1 :. (a2 :. ()))
--vec3 x y z = x Vec.:. y Vec.:. z Vec.:. ()
mvpMatrix :: Vec.Mat44 GL.GLfloat
mvpMatrix = Vec.multmm (Vec.multmm projection view) model
  where
    projection = Vec.perspective 0.1 100 (pi / 4) (4 / 3)
    view = Vec.identity :: Vec.Mat44 GL.GLfloat --lookAt (vec3 4 3 3) (vec3 0 0 0) (vec3 0 1 0)
    model = Vec.identity :: Vec.Mat44 GL.GLfloat

setMVP :: GL.Program -> Vec.Mat44 GL.GLfloat -> IO ()
setMVP program mvp = do
  GL.UniformLocation mvpUniform <- GL.uniformLocation program "MVP"
  with mvp $ GLF.glUniformMatrix4fv mvpUniform 1 (fromBool True) . Ptr.castPtr

view :: Shaders -> MyState -> IO ()
view shaders (MyState primitives color _) = do
  GL.clearColor $= GL.Color4 1 0 0 1
  GL.clear [GL.ColorBuffer]
  program <- activateProgram shaders SimpleFragment
  setColor program color
  setMVP program mvpMatrix
  mapM_ draw primitives
view _ Empty = return ()

keymap =
  [ (KeyAction GLFW.Key'Escape GLFW.KeyState'Pressed noModifiers, run shutdown)
  , (KeyAction GLFW.Key'Space GLFW.KeyState'Pressed noModifiers, run swapColor)
  , ( KeyAction GLFW.Key'Space GLFW.KeyState'Repeating noModifiers
    , run swapColor)
  ]

initialAction :: MyState -> Action
initialAction state =
  Action $ \_ -> do
    askTime
    return state

setup = do
  square <- create
  return $ initialAction $ MyState [square] Green 0

initState = do
  keyPresses keymap
  runIO setup
  return Empty

myApplication = Application runAction view

main :: IO ()
main = start DefaultConfig initState myApplication
