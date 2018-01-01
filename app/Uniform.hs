module Uniform
  ( setUniform
  , setUniform4fv
  , getUniform
  ) where

import           Foreign.Marshal
import qualified Graphics.GL.Functions       as GLF
import           Graphics.Rendering.OpenGL   (($=))
import qualified Graphics.Rendering.OpenGL   as GL
import           Matrix
import           Numeric.LinearAlgebra
import qualified Numeric.LinearAlgebra.Devel as D

setUniform program name value = do
  uniform <- GL.uniformLocation program name
  GL.uniform uniform $= value

setUniform4fv :: Matrix4 -> GL.UniformLocation -> IO ()
setUniform4fv value (GL.UniformLocation uniform) =
  withFloatMatrix value $ \order rows cols ptr ->
    GLF.glUniformMatrix4fv uniform 1 (fromBool (order == D.RowMajor)) ptr

getUniform :: GL.Program -> String -> IO GL.UniformLocation
getUniform = GL.uniformLocation
