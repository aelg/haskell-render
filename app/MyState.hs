{-# LANGUAGE TemplateHaskell #-}
module MyState
  ( MyState
  , Color(..)
  , initialState
  , squares
  , cubes
  , color
  , lastSecond
  , squarePos
  , cameraPos
  , aspectRatio
  ) where

import           Numeric.LinearAlgebra
import           Primitives.Cube
import           Primitives.Square
import           Lens.Micro.TH

--State
data Color
  = Red
  | Green
  | Blue
  deriving (Show)

data MyState
  = MyState { _squares     :: [Square]
            , _cubes       :: [Cube]
            , _color       :: Color
            , _lastSecond  :: Double
            , _squarePos   :: Vector Double
            , _cameraPos   :: Vector Double
            , _aspectRatio :: Double }
  deriving (Show)

makeLenses ''MyState

initialState =
  MyState
  { _squares = []
  , _cubes = []
  , _color = Green
  , _lastSecond = 0
  , _squarePos = vector [0, 0, 0]
  , _cameraPos = vector [0, 0, 10.0]
  , _aspectRatio = 4 / 3
  }
