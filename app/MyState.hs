{-# LANGUAGE TemplateHaskell #-}

module MyState
  ( MyState
  , Color(..)
  , initialState
  , squares
  , cubes
  , terrain
  , color
  , nextSecond
  , cubePos
  , camera
  , aspectRatio
  , lastTime
  , movementSpeed
  , spacing
  , Camera
  , cameraRotation
  , cameraPosition
  ) where

import           Lens.Micro.TH

import           Primitives.Cube
import           Primitives.Square
import           Primitives.Terrain

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

initialCamera = Camera (0, 0) (vec3 0 3 10)

makeLenses ''Camera

data MyState = MyState
  { _squares       :: [Square]
  , _cubes         :: [Cube]
  , _terrain       :: [Terrain]
  , _color         :: Color
  , _nextSecond    :: Double
  , _cubePos       :: Vector3
  , _camera        :: Camera
  , _aspectRatio   :: Double
  , _lastTime      :: Double
  , _movementSpeed :: Double
  , _spacing       :: Double
  } deriving (Show)

makeLenses ''MyState

initialState =
  MyState
  { _squares = []
  , _cubes = []
  , _terrain = []
  , _color = Green
  , _nextSecond = 0
  , _cubePos = vec3 0 0 0
  , _camera = initialCamera
  , _aspectRatio = 4 / 3
  , _lastTime = 0
  , _movementSpeed = 80
  , _spacing = 0
  }
