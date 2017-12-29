module Main
  ( main
  ) where

import           Control.Monad
import qualified Graphics.UI.GLFW      as GLFW
import           Lens.Micro.Platform   ((%~), (&), (.~), (^.))
import           Numeric.LinearAlgebra

import           Application
import           Cmd
import           Initializable
import           Keyboard
import           Matrix
import           MyState
import           Primitives.Cube
import           Primitives.Square
import           View

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
run = Action

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
swapColor state = return $ state & color %~ blueGreen

askTime :: Update ()
askTime = getTime (run timeFail) (run1 gotTime)

gotTime :: Double -> MyState -> Update MyState
gotTime a state = do
  askTime
  if a > state ^. lastSecond
    then do
      send $ run swapColor
      return $ state & lastSecond %~ (+ 1)
    else return state

timeFail :: MyState -> Update MyState
timeFail state = do
  askTime
  doPrint "Time: failed"
  return state

moveSquare dir state = return $ state & squarePos %~ (+ dir)

pressedArrow (KeyPress GLFW.Key'Up _ _) = run $ moveSquare $ vec3 0 0.2 0
pressedArrow (KeyPress GLFW.Key'Down _ _) = run $ moveSquare $ vec3 0 (-0.2) 0
pressedArrow (KeyPress GLFW.Key'Left _ _) = run $ moveSquare $ vec3 (-0.2) 0 0
pressedArrow (KeyPress GLFW.Key'Right _ _) = run $ moveSquare $ vec3 0.2 0 0
pressedArrow _ = noRun

moveCamera dir state = return $ state & camera . cameraPosition %~ (+ dir)

pressedWASD (KeyPress GLFW.Key'W _ _) = run $ moveCamera $ vec3 0 0 (-0.2)
pressedWASD (KeyPress GLFW.Key'S _ _) = run $ moveCamera $ vec3 0 0 0.2
pressedWASD (KeyPress GLFW.Key'A _ _) = run $ moveCamera $ vec3 (-0.2) 0 0
pressedWASD (KeyPress GLFW.Key'D _ _) = run $ moveCamera $ vec3 0.2 0 0
pressedWASD _                         = noRun

mouseMoved x y state = do
  doPrint $ "Mouse moved " ++ show x ++ " " ++ show y
  return state

resize' :: Int -> Int -> MyState -> Update MyState
resize' w h state =
  return $ state & aspectRatio .~ fromIntegral w / fromIntegral h

keymap =
  concat
    [ [KeyPressed GLFW.Key'Escape (\_ -> run shutdown)]
    , [ KeyPressed
          GLFW.Key'M
          (\_ -> run (\state -> stopCaptureMouse >> return state))
      ]
    , KeyState <$> [GLFW.Key'Space] <*>
      [GLFW.KeyState'Pressed, GLFW.KeyState'Repeating] <*>
      [\_ -> run swapColor]
    , KeyState <$> [GLFW.Key'Up, GLFW.Key'Down, GLFW.Key'Right, GLFW.Key'Left] <*>
      [GLFW.KeyState'Pressed, GLFW.KeyState'Repeating] <*>
      [pressedArrow]
    , KeyState <$> [GLFW.Key'W, GLFW.Key'A, GLFW.Key'S, GLFW.Key'D] <*>
      [GLFW.KeyState'Pressed, GLFW.KeyState'Repeating] <*>
      [pressedWASD]
    ]

initialAction :: MyState -> Action
initialAction state =
  Action $ \_ -> do
    askTime
    return state

setup = do
  square <- create
  cube <- create
  let state = initialState & squares .~ [square] & cubes .~ [cube]
  return $ initialAction state

initState = do
  keyPresses keymap
  resize $ run2 resize'
  runIO setup
  captureMouse $ run2 mouseMoved
  return initialState

myApplication = Application runAction view

main :: IO ()
main = start DefaultConfig initState myApplication
