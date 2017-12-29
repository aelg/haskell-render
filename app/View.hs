module View
  ( view
  ) where

import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import           Matrix
import           Numeric.LinearAlgebra
import           Lens.Micro.Platform ((^.))

import           Drawable
import           MyState
import           Shaders
import           Uniform

getColor Red   = GL.Color3 1.0 0.0 (0.0 :: GL.GLfloat)
getColor Green = GL.Color3 0.0 1.0 (0.0 :: GL.GLfloat)
getColor Blue  = GL.Color3 0.0 0.0 (1.0 :: GL.GLfloat)

--View
setColor program color = setUniform program "color" $ getColor color

pv aspectRatio cameraPos = projection <> viewMatrix cameraPos
  where
    projection = perspective 0.1 100.0 (pi / 4.0) aspectRatio

viewMatrix :: Vector Double -> Matrix Double
viewMatrix = translate . cmap negate

mvpMatrix aspectRatio cameraPos squareP =
  pv aspectRatio cameraPos <> model squareP

model :: Vector Double -> Matrix Double
model = translate

lightPosition = GL.Vertex3 0.0 0.0 1.01 :: GL.Vertex3 GL.GLfloat

reset =
  GL.clearColor $= GL.Color4 0 0 0 1 >>
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

view :: Shaders -> MyState -> IO ()
view shaders state = do
  reset
  program <- activateProgram shaders Normal
  setColor program $ state ^. color
  setUniform program "useVertexColor" (1 :: GL.GLuint)
  setUniform program "LightPosition_worldspace" lightPosition
  setUniform4fv program "M" (model $ state ^. squarePos)
  setUniform4fv program "V" (viewMatrix $ state ^. cameraPos)
  setUniform4fv program "MVP" (mvpMatrix (state ^. aspectRatio) (state ^. cameraPos) (state ^. squarePos))
  mapM_ draw $ state ^. cubes
