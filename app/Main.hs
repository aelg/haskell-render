module Main
  ( main
  ) where

import           Control.Monad
import qualified Graphics.UI.GLFW      as GLFW
import           Numeric.LinearAlgebra

import           Application
import           Cmd
import           Initializable
import           Keyboard
import           MyState
import           Primitives.Cube
import           Primitives.Square
import           View

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
  { runAction :: MyState -> Update MyState
  }

type Update a = State (Cmd Action) a

-- Update
blueGreen Blue  = Green
blueGreen Green = Blue

noRun :: Action
noRun = Action return

run :: (MyState -> Update MyState) -> Action
run f = Action f

run1 :: (a -> MyState -> Update MyState) -> a -> Action
run1 f a = Action (f a)

run2 :: (a -> b -> MyState -> Update MyState) -> a -> b -> Action
run2 f a b = Action (f a b)

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

moveSquare dir (state@MyState {squarePos = p}) =
  return $ state {squarePos = p + dir}

pressedArrow (KeyPress GLFW.Key'Up _ _) = run $ moveSquare $ vector [0, 0.2, 0]
pressedArrow (KeyPress GLFW.Key'Down _ _) =
  run $ moveSquare $ vector [0, -0.2, 0]
pressedArrow (KeyPress GLFW.Key'Left _ _) =
  run $ moveSquare $ vector [-0.2, 0, 0]
pressedArrow (KeyPress GLFW.Key'Right _ _) =
  run $ moveSquare $ vector [0.2, 0, 0]
pressedArrow _ = noRun

resize' :: Int -> Int -> MyState -> Update MyState
resize' w h state =
  return $ state {aspectRatio = fromIntegral w / fromIntegral h}

keymap =
  concat
    [ [KeyPressed GLFW.Key'Escape (\_ -> run shutdown)]
    , KeyState <$> [GLFW.Key'Space] <*>
      [GLFW.KeyState'Pressed, GLFW.KeyState'Repeating] <*>
      [\_ -> run swapColor]
    , KeyState <$> [GLFW.Key'Up, GLFW.Key'Down, GLFW.Key'Right, GLFW.Key'Left] <*>
      [GLFW.KeyState'Pressed, GLFW.KeyState'Repeating] <*>
      [pressedArrow]
    ]

initialAction :: MyState -> Action
initialAction state =
  Action $ \_ -> do
    askTime
    return state

setup = do
  square <- create
  cube <- create
  let state = initialState {square = [square], cube = [cube]}
  return $ initialAction state

initState = do
  keyPresses keymap
  resize $ run2 resize'
  runIO setup
  return Empty

myApplication = Application runAction view

main :: IO ()
main = start DefaultConfig initState myApplication
