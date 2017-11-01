module Display (loop) where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Control.Monad
import qualified Foreign.Ptr as Ptr

import Descriptor
import Square

draw :: Descriptor -> IO ()
draw (Descriptor square firstIndex numVertices) = do 
  let firstIndex = 0
      vPosition = AttribLocation 0
  vertexAttribPointer vPosition $= 
    (ToFloat, VertexArrayDescriptor 2 Float 0 (bufferOffset firstIndex))
  vertexAttribArray vPosition $= Enabled
  bindVertexArrayObject $= Just square
  drawArrays Triangles firstIndex numVertices
  where bufferOffset = Ptr.plusPtr Ptr.nullPtr . fromIntegral

onDisplay :: Window -> Descriptor -> IO ()
onDisplay win square = do
  GL.clearColor $= Color4 1 0 0 1
  GL.clear [ColorBuffer]
  draw square
  GLFW.swapBuffers win


loop :: Window -> Descriptor -> IO ()
loop win square = 
  forever $ do
    GLFW.pollEvents
    onDisplay win square

