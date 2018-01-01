{-# LANGUAGE DataKinds #-}

module Rotation
  ( Rotation
  , rotationMatrix
  , rotate
  , mkRotation
  , mkXYRotation
  , mkXYRotationCamera
  , rotateV
  ) where

import qualified Numeric.LinearAlgebra        as L
import           Numeric.LinearAlgebra.Static

import           Matrix

type Quaternion = R 4

type Rotation = Quaternion

rotate :: Quaternion -> Quaternion -> Quaternion
q1 `rotate` q2 =
  vec4
    (r1 * r2 - i1 * i2 - j1 * j2 - k1 * k2)
    (r1 * i2 + i1 * r2 + j1 * k2 - k1 * j2)
    (r1 * j2 + j1 * r2 + k1 * i2 - i1 * k2)
    (r1 * k2 + k1 * r2 + i1 * j2 - j1 * i2)
  where
    [r1, i1, j1, k1] = L.toList . unwrap $ q1
    [r2, i2, j2, k2] = L.toList . unwrap $ q2

mkRotation :: Double -> Vector3 -> Quaternion
mkRotation angle axis =
  normalize $
  vec4
    (cos (angle / 2))
    (x * sin (angle / 2))
    (y * sin (angle / 2))
    (z * sin (angle / 2))
  where
    [x, y, z] = L.toList . unwrap $ axis

rotationMatrix :: Quaternion -> Matrix4
rotationMatrix q = eye + 2 * m
  where
    [r, i, j, k] = L.toList . unwrap $ q
    m =
      row (vec4 (-j * j - k * k) (i * j - k * r) (i * k + j * r) 0) ===
      row (vec4 (i * j + k * r) (-i * i - k * k) (j * k - i * r) 0) ===
      row (vec4 (i * k - j * r) (j * k + i * r) (-i * i - j * j) 0) ===
      row (vec4 0 0 0 0)

mkXYRotationCamera x y = q2 `rotate` q1
  where
    q1 = mkRotation x yHat
    q2 = mkRotation y xHat

mkXYRotation x y = q2 `rotate` q1
  where
    q1 = mkRotation y xHat
    q2 = mkRotation x yHat

conj :: Quaternion -> Quaternion
conj q = q * vec4 1 (-1) (-1) (-1)

qmult = rotate

rotateV :: Vector3 -> Quaternion -> Vector3
rotateV v q = v'
  where
    (_, v') = split $ q `qmult` p `qmult` conj q
    p = vector [0] # v
