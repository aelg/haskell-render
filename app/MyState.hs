module MyState
  ( MyState(..)
  , Color(..)
  , initialState
  ) where

import           Numeric.LinearAlgebra
import           Primitives.Square
import           Primitives.Cube

--State
data Color
  = Red
  | Green
  | Blue
  deriving (Show)

data MyState
  = Empty
  | MyState { square     :: [Square]
            , cube       :: [Cube]
            , color      :: Color
            , lastSecond :: Double
            , squarePos  :: Vector Double }
  deriving (Show)

initialState =
  MyState
  {square = [], cube = [], color = Green, lastSecond = 0, squarePos = vector [1, 0, 0]}
