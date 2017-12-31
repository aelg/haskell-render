module View
  ( view
  ) where

import           Graphics.Rendering.OpenGL    (($=))
import qualified Graphics.Rendering.OpenGL    as GL
import           Lens.Micro.Platform          ((^.))

--import           Numeric.LinearAlgebra
import           Numeric.LinearAlgebra.Static

import           Drawable
import           Matrix
import           MyState
import           Rotation
import           Shaders
import           Uniform

getColor Red   = GL.Color3 1.0 0.0 (0.0 :: GL.GLfloat)
getColor Green = GL.Color3 0.0 1.0 (0.0 :: GL.GLfloat)
getColor Blue  = GL.Color3 0.0 0.0 (1.0 :: GL.GLfloat)

--View
setColor program color = setUniform program "color" $ getColor color

viewMatrix :: Camera -> Matrix4
viewMatrix camera = rot <> translation
  where
    translation = translate . (* (-1)) $ camera ^. cameraPosition
    rot = rotationMatrix $ mkXYRotation (-x) (-y)
    (x, y) = camera ^. cameraRotation

model :: Vector3 -> Matrix4
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
  let m = model $ state ^. cubePos
      v = viewMatrix $ state ^. camera
      p = projection $ state ^. aspectRatio
      mvp = p <> v <> m
  setUniform4fv program "M" m
  setUniform4fv program "V" v
  setUniform4fv program "MVP" mvp
  mapM_ draw $ state ^. cubes
