{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

module Display
  ( loop
  ) where

import           Control.Monad
import           Data.Vec                  ((:.))
import qualified Data.Vec                  as Vec
import           Foreign.Marshal
import qualified Foreign.Ptr               as Ptr
import qualified Foreign.Storable          as S
import qualified Graphics.GL.Functions     as GLF
import           Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW

import           Drawable
import           Shaders
import           Square

green = Color3 0.0 1.0 (0.0 :: GLfloat)

setColor program color = do
  colorUniform <- uniformLocation program "color"
  uniform colorUniform $= color

vec3 :: forall a a1 a2. a -> a1 -> a2 -> a :. (a1 :. (a2 :. ()))
vec3 x y z = x Vec.:. y Vec.:. z Vec.:. ()

mvpMatrix :: Vec.Mat44 GL.GLfloat
mvpMatrix = Vec.multmm (Vec.multmm projection view) model
  where
    projection = Vec.perspective 0.1 100 (pi / 4) (4 / 3)
    view = Vec.identity :: Vec.Mat44 GLfloat --lookAt (vec3 4 3 3) (vec3 0 0 0) (vec3 0 1 0)
    model = Vec.identity :: Vec.Mat44 GLfloat

setMVP :: GL.Program -> Vec.Mat44 GL.GLfloat -> IO ()
setMVP program mvp = do
  GL.UniformLocation mvpUniform <- uniformLocation program "MVP"
  with mvp $ GLF.glUniformMatrix4fv mvpUniform 1 (fromBool True) . Ptr.castPtr

onDisplay :: Drawable a => GLFW.Window -> Shaders -> [a] -> IO ()
onDisplay win shaders primitives = do
  GL.clearColor $= Color4 1 0 0 1
  GL.clear [ColorBuffer]
  program <- activateProgram shaders SimpleFragment
  setColor program green
  setMVP program mvpMatrix
  mapM_ draw primitives
  GLFW.swapBuffers win

loop :: Drawable a => GLFW.Window -> Shaders -> [a] -> IO ()
loop win shaders primitives =
  forever $ do
    GLFW.pollEvents
    onDisplay win shaders primitives
