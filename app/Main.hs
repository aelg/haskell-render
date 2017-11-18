module Main
  ( module Main
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
import           Machine

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
            , color      :: Color }
  deriving (Show)

--Actions
data MyAction
  = Frame
  | SwapColor
  | Shutdown
  | Spacebar
  | Initial MyState
  deriving (Show)

-- Update
swapColor Blue  = Green
swapColor Green = Blue

updatePrinter :: MyState -> MyAction -> State MyAction MyState
updatePrinter a b = do
  doPrint $ show b
  update a b

update :: MyState -> MyAction -> State MyAction MyState
update state Frame = return state
update state Shutdown = do
  doPrint "Will shutdown"
  doShutdown
  return state
update (state@MyState {color = c}) SwapColor = do
  return $ state {color = swapColor c}
update _ (Initial state) = return state

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
view shaders (MyState primitives color) = do
  GL.clearColor $= GL.Color4 1 0 0 1
  GL.clear [GL.ColorBuffer]
  program <- activateProgram shaders SimpleFragment
  setColor program color
  setMVP program mvpMatrix
  mapM_ draw primitives

keymap =
  [ (KeyAction GLFW.Key'Escape GLFW.KeyState'Pressed noModifiers, Shutdown)
  , (KeyAction GLFW.Key'Space GLFW.KeyState'Pressed noModifiers, SwapColor)
  ]

setup = do
  square <- create
  return $ Initial $ MyState [square] Green

initState = do
  keyPresses keymap
  runIO setup
  return Empty

myApplication = Application updatePrinter view Frame

main :: IO ()
main = run DefaultConfig initState myApplication
