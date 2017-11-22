module View
  ( view
  ) where

import           Foreign.Marshal
import qualified Foreign.Ptr               as Ptr
import qualified Graphics.GL.Functions     as GLF
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import           Matrix
import           Numeric.LinearAlgebra

import           Drawable
import           MyState
import           Shaders

getColor Red   = GL.Color3 1.0 0.0 (0.0 :: GL.GLfloat)
getColor Green = GL.Color3 0.0 1.0 (0.0 :: GL.GLfloat)
getColor Blue  = GL.Color3 0.0 0.0 (1.0 :: GL.GLfloat)

--View
setColor program color = do
  colorUniform <- GL.uniformLocation program "color"
  GL.uniform colorUniform $= getColor color

pv aspectRatio = projection <> view
  where
    projection = perspective 0.1 100.0 (pi / 4.0) aspectRatio
    view = translate $ vector [0, 0, -5] :: Matrix Double

mvpMatrix aspectRatio pos = pv aspectRatio <> model
  where
    model = translate pos :: Matrix Double

setMVP :: GL.Program -> Matrix Double -> IO ()
setMVP program mvp = do
  GL.UniformLocation mvpUniform <- GL.uniformLocation program "MVP"
  withFloatMatrix mvp (setUniform mvpUniform)
  where
    setUniform uniform row cols =
      GLF.glUniformMatrix4fv uniform 1 (fromBool False) . Ptr.castPtr

view :: Shaders -> MyState -> IO ()
view shaders (MyState square cube color _ squarePos aspectRatio) = do
  GL.clearColor $= GL.Color4 1 0 0 1
  GL.clear [GL.ColorBuffer]
  program <- activateProgram shaders SimpleFragment
  setColor program color
  setMVP program (mvpMatrix aspectRatio squarePos)
  --mapM_ draw square
  setMVP program (mvpMatrix aspectRatio squarePos)
  mapM_ draw cube
view _ Empty = return ()
