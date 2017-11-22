module Matrix
  ( Matrix
  , perspective
  , identity
  , mmult
  , withFloatMatrix
  , translate
  ) where

import           Control.Monad.ST
import           Data.List
import           Foreign.C.Types
import           Foreign.Ptr
import           Numeric.LinearAlgebra
import qualified Numeric.LinearAlgebra.Devel as D

--type Matrix = Matrix Double
perspective :: Double -> Double -> Double -> Double -> Matrix Double
perspective near far fov aspect = build (4, 4) gen
  where
    gen i j =
      case (i, j) of
        (0, 0) -> 1.0 / (aspect * tan (fov / 2.0))
        (1, 1) -> 1.0 / tan (fov / 2.0)
        (2, 2) -> -(far + near) / (far - near)
        (2, 3) -> -2.0 * far * near / (far - near)
        (3, 2) -> -1.0
        _      -> 0.0

identity :: Int -> Matrix Double
identity = ident

translate :: Vector Double -> Matrix Double
translate v = ident 4 + build (4, 4) gen
  where
    [x, y, z] = toList v
    gen i j =
      case (i, j) of
        (0, 3) -> x
        (1, 3) -> y
        (2, 3) -> z
        _      -> 0.0

mmult a b = a <> b

withFloatMatrix :: Matrix Double -> (CInt -> CInt -> Ptr Float -> IO r) -> IO r
withFloatMatrix m = D.applyRaw (single m) id
