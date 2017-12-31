module View
  ( view
  ) where

import           Data.Maybe
import           Graphics.Rendering.OpenGL    (($=))
import qualified Graphics.Rendering.OpenGL    as GL
import qualified Graphics.UI.GLFW             as GLFW
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

lightPosition = GL.Vertex3 (-0.0) 0.0 0.0 :: GL.Vertex3 GL.GLfloat

reset =
  GL.clearColor $= GL.Color4 0 0 0 1 >>
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

drawCube mU pU state pos = do
  let m = model pos
  seq m $ return ()
  setUniform4fv mU m
  draw . head $ state ^. cubes

grid space =
  map
    (* realToFrac space)
    (vec3 <$> [-10 .. 10] <*> [-10 .. 10] <*> [-10 .. 10])

view :: Shaders -> MyState -> IO ()
view shaders state = do
  startTime <- GLFW.getTime
  reset
  program <- activateProgram shaders Normal
  setColor program $ state ^. color
  setUniform program "useVertexColor" (1 :: GL.GLuint)
  setUniform program "LightPosition_worldspace" lightPosition
  let m = model $ state ^. cubePos
      v = viewMatrix $ state ^. camera
      p = projection $ state ^. aspectRatio
      vp = p <> v <> m
  mU <- getUniform program "M"
  vU <- getUniform program "V"
  pU <- getUniform program "P"
  setUniform4fv vU v
  setUniform4fv pU p
  mapM_ (drawCube mU pU state) (grid $ state ^. spacing)
  endTime <- GLFW.getTime
  let fps = ((1 /) . fromJust) ((-) <$> endTime <*> startTime)
  putStrLn $ "FPS: " ++ show fps
  putStrLn $
    "Triangles per second: " ++ show (fromIntegral (length (grid 3)) * 12 * fps)
