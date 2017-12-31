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
  , camera
  , aspectRatio
  , Camera
  , cameraRotation
  , cameraPosition
  ) where

import           Lens.Micro.TH

import           Primitives.Cube
import           Primitives.Square

import           Matrix
import           Rotation

--State
data Color
  = Red
  | Green
  | Blue
  deriving (Show)

data Camera = Camera
  { _cameraRotation :: (Double, Double)
  , _cameraPosition :: Vector3
  } deriving (Show)

newtype WASD =
  WASD (Bool, Bool, Bool, Bool)
  deriving (Show)

makeLenses ''WASD

initialCamera = Camera (0, 0) (vec3 0 0 10)

makeLenses ''Camera

data MyState = MyState
  { _squares     :: [Square]
  , _cubes       :: [Cube]
  , _color       :: Color
  , _lastSecond  :: Double
  , _squarePos   :: Vector3
  , _camera      :: Camera
  , _aspectRatio :: Double
  } deriving (Show)

makeLenses ''MyState

initialState =
  MyState
  { _squares = []
  , _cubes = []
  , _color = Green
  , _lastSecond = 0
  , _squarePos = vec3 0 0 0
  , _camera = initialCamera
  , _aspectRatio = 4 / 3
  }
