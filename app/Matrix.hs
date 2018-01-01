{-# LANGUAGE DataKinds #-}

module Matrix
  ( Matrix4
  , Vector3
  , vec3
  , perspective
  , identity
  , withFloatMatrix
  , translate
  , projection
  , normalize
  , xHat
  , yHat
  , zHat
  , cross
  , vector3ToFloatList
  , unwrap
  ) where

import           Data.Maybe
import           Foreign.C.Types
import           Foreign.Ptr
import qualified Numeric.LinearAlgebra        as L
import qualified Numeric.LinearAlgebra.Devel  as D
import           Numeric.LinearAlgebra.Static

type Matrix4 = L 4 4

type Vector3 = R 3

xHat = vec3 1 0 0

yHat = vec3 0 1 0

zHat = vec3 0 0 1

--type Matrix4 = Matrix4 Double
perspective :: Double -> Double -> Double -> Double -> Matrix4
perspective near far fov aspect = build gen
  where
    gen i j =
      case (i, j) of
        (0, 0) -> 1.0 / (aspect * tan (fov / 2.0))
        (1, 1) -> 1.0 / tan (fov / 2.0)
        (2, 2) -> -(far + near) / (far - near)
        (2, 3) -> -2.0 * far * near / (far - near)
        (3, 2) -> -1.0
        _      -> 0.0

identity :: Matrix4
identity = eye

translate :: Vector3 -> Matrix4
translate v = t ||| col (v & 1.0)
  where
    (t, _) = splitCols eye

projection :: Double -> Matrix4
projection = perspective 0.1 10000.0 (pi / 4.0)

withFloatMatrix ::
     Matrix4 -> (D.MatrixOrder -> CInt -> CInt -> Ptr Float -> IO r) -> IO r
withFloatMatrix m f = D.applyRaw singleM id (f (D.orderOf singleM))
  where
    singleM = L.single $ unwrap m

withFloatVector :: Vector3 -> (CInt -> Ptr Float -> IO r) -> IO r
withFloatVector m = D.applyRaw singleV id
  where
    singleV = L.single $ unwrap m

--normalize :: Vector3 -> Vector3
normalize v = realToFrac a * v
  where
    a = 1.0 / norm_2 v

vector3ToFloatList :: Vector3 -> [Float]
vector3ToFloatList = L.toList . L.single . unwrap
