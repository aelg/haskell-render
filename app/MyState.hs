module MyState
  ( MyState(..)
  , Color(..)
  , initialState
  ) where

import           Numeric.LinearAlgebra
import           Primitives.Cube
import           Primitives.Square

--State
data Color
  = Red
  | Green
  | Blue
  deriving (Show)

data MyState
  = Empty
  | MyState { square      :: [Square]
            , cube        :: [Cube]
            , color       :: Color
            , lastSecond  :: Double
            , squarePos   :: Vector Double
            , aspectRatio :: Double }
  deriving (Show)

initialState =
  MyState
  { square = []
  , cube = []
  , color = Green
  , lastSecond = 0
  , squarePos = vector [0, 0, 0]
  , aspectRatio = 4 / 3
  }
