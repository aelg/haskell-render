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

setUniform program name value = do
  uniform <- GL.uniformLocation program name
  GL.uniform uniform $= value

setUniformv :: GL.Program -> String -> Matrix Double -> IO ()
setUniformv program name value = do
  GL.UniformLocation uniform <- GL.uniformLocation program name
  withFloatMatrix (value) $ \rows cols -> GLF.glUniformMatrix4fv uniform 1 (fromBool False) . Ptr.castPtr

pv aspectRatio cameraPos = projection <> viewMatrix cameraPos
  where
    projection = perspective 0.1 100.0 (pi / 4.0) aspectRatio

viewMatrix pos = translate $ pos :: Matrix Double

mvpMatrix aspectRatio cameraPos squareP = pv aspectRatio cameraPos <> (model squareP)

model :: Vector Double -> Matrix Double
model pos = translate pos

setMVP :: GL.Program -> Matrix Double -> IO ()
setMVP program mvp = do
  GL.UniformLocation mvpUniform <- GL.uniformLocation program "MVP"
  withFloatMatrix mvp (setUniform mvpUniform)
  where
    setUniform uniform row cols =
      GLF.glUniformMatrix4fv uniform 1 (fromBool False) . Ptr.castPtr

squareP = vector [0.0, 0.0, (-5.0)] :: Vector Double

view :: Shaders -> MyState -> IO ()
view shaders (MyState square cube color _ cameraPos aspectRatio) = do
  GL.clearColor $= GL.Color4 0 0 0 1
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  program <- activateProgram shaders Normal
  setColor program color
  setUniform program "useVertexColor" (1 :: GL.GLuint)
  setUniform program "LightPosition_worldspace" (GL.Vertex3 0.0 0.0 (-2.0) :: GL.Vertex3 GL.GLfloat)
  setUniformv program "M" (model squareP)
  setUniformv program "V" (viewMatrix cameraPos)
  setUniformv program "P" (perspective 0.1 100.0 (pi/4.0) aspectRatio)
  --mapM_ draw square
  setMVP program (mvpMatrix aspectRatio cameraPos squareP)
  mapM_ draw cube
view _ Empty = return ()
