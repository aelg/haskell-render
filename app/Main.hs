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

cameraMovement timeDiff = do
  keyPress GLFW.Key'W (run $ moveCamera timeDiff forward) noRun
  keyPress GLFW.Key'S (run $ moveCamera timeDiff (negate . forward)) noRun
  keyPress GLFW.Key'A (run $ moveCamera timeDiff leftward) noRun
  keyPress GLFW.Key'D (run $ moveCamera timeDiff (negate . leftward)) noRun
  keyPress GLFW.Key'Space (run $ moveCamera timeDiff upward) noRun
  keyPress GLFW.Key'C (run $ moveCamera timeDiff (negate . upward)) noRun

cubeMovement timeDiff = do
  keyPress GLFW.Key'Up (run $ moveSquare timeDiff yHat) noRun
  keyPress GLFW.Key'Down (run $ moveSquare timeDiff (-yHat)) noRun
  keyPress GLFW.Key'Right (run $ moveSquare timeDiff xHat) noRun
  keyPress GLFW.Key'Left (run $ moveSquare timeDiff (-xHat)) noRun

updateSpacing t = ((sin (t / 2) + 1) * 5) + 2

gotTime :: Double -> MyState -> Update MyState
gotTime t state = do
  askTime
  let timeDiff = t - state ^. lastTime
  cameraMovement timeDiff
  cubeMovement timeDiff
  return $ state & lastTime .~ t & spacing .~ updateSpacing t

timeFail :: MyState -> Update MyState
timeFail state = do
  askTime
  doPrint "Time: failed"
  return state

moveSquare distance dir state =
  return $ state & cubePos %~
  (+ (dir * realToFrac (state ^. movementSpeed) * realToFrac distance))

moveCamera distance dirF state =
  return $ state & camera . cameraPosition %~
  (+ (dirF state * realToFrac (state ^. movementSpeed) * realToFrac distance))

forward state = rotateV (negate zHat) $ mkXYRotation x y
  where
    (x, y) = state ^. camera . cameraRotation

leftward state = negate $ cross (forward state) yHat

upward state = yHat

mouseMoved x y state
  | abs x < 300 && abs y < 300 =
    return $! state & camera . cameraRotation %~
    (first (+ (negate x * 0.0001)) >>> second (+ (negate y * 0.0001)))
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
  terrain' <- create
  let state =
        initialState & squares .~ [square] & cubes .~ [cube] & terrain .~
        [terrain']
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
