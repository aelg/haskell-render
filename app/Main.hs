module Main
  ( main
  ) where

import           Control.Arrow
import           Control.Monad
import qualified Graphics.UI.GLFW    as GLFW
import           Lens.Micro.Platform ((%~), (&), (.~), (^.))

--import           Numeric.LinearAlgebra
import           Application
import           Cmd
import           Initializable
import           Keyboard
import           Matrix
import           MyState
import           Primitives.Cube
import           Primitives.Square
import           Rotation
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

cameraMovement = do
  keyPress GLFW.Key'W (run $ moveCamera forward) noRun
  keyPress GLFW.Key'S (run $ moveCamera (negate . forward)) noRun
  keyPress GLFW.Key'A (run $ moveCamera leftward) noRun
  keyPress GLFW.Key'D (run $ moveCamera (negate . leftward)) noRun

cubeMovement = do
  keyPress GLFW.Key'Up (run $ moveSquare yHat) noRun
  keyPress GLFW.Key'Down (run $ moveSquare (-yHat)) noRun
  keyPress GLFW.Key'Right (run $ moveSquare xHat) noRun
  keyPress GLFW.Key'Left (run $ moveSquare (-xHat)) noRun

gotTime :: Double -> MyState -> Update MyState
gotTime a state = do
  askTime
  cameraMovement
  cubeMovement
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

moveSquare dir state = return $ state & cubePos %~ (+ (dir * 0.2))

moveCamera dir state =
  return $ state & camera . cameraPosition %~ (+ (dir state * 0.2))

forward state = rotateV (negate zHat) $ mkXYRotation x y
  where
    (x, y) = state ^. camera . cameraRotation

leftward state = negate $ cross (forward state) yHat

mouseMoved x y state
  | abs x < 30 && abs y < 30 =
    return $! state & camera . cameraRotation %~
    (first (+ (negate x * 0.0005)) >>> second (+ (negate y * 0.0005)))
  | otherwise = return state

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
