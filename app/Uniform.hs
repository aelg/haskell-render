module Uniform (setUniform, setUniform4fv) where


import           Foreign.Marshal
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import           Numeric.LinearAlgebra
import qualified Numeric.LinearAlgebra.Devel as D
import qualified Graphics.GL.Functions     as GLF
import           Matrix

setUniform program name value = do
  uniform <- GL.uniformLocation program name
  GL.uniform uniform $= value

setUniform4fv :: GL.Program -> String -> Matrix Double -> IO ()
setUniform4fv program name value = do
  GL.UniformLocation uniform <- GL.uniformLocation program name
  withFloatMatrix value $ \order rows cols ptr ->
    GLF.glUniformMatrix4fv uniform 1 (fromBool (order == D.RowMajor)) ptr