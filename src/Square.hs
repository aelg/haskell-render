module Square (Square(Square)) where

import Descriptor
import Initializable
import Drawable
import Graphics.Rendering.OpenGL
import Foreign.Marshal.Array
import qualified Foreign.Storable as S
import qualified Foreign.Ptr as Ptr

data Square = Square | SquareData VertexArrayObject

instance Initializable Square where
  create Square = do
    square <- genObjectName
    bindVertexArrayObject $= Just square

    let vertices = map (\(Vertex2 x y) -> Vertex2 (x*0.8) (y*0.8))
                   [ Vertex2 1.0 (-1.0)
                   , Vertex2 1.0 1.0
                   , Vertex2 (-1.0) 1.0
                   , Vertex2 1.0 (-1.0)
                   , Vertex2 (-1.0) 1.0
                   , Vertex2 (-1.0) (-1.0)
                   ] :: [Vertex2 GLfloat]
        numVertices = length vertices
        firstIndex = 0
        vPosition = AttribLocation 0

    arrayBuffer <- genObjectName
    bindBuffer ArrayBuffer $= Just arrayBuffer
    withArray vertices $ \ptr -> do
      let size = fromIntegral (numVertices * S.sizeOf (head vertices))
      bufferData ArrayBuffer $= (size, ptr, StaticDraw)

    vertexAttribPointer vPosition $=
      (ToFloat, VertexArrayDescriptor 2 Float 0 (bufferOffset firstIndex))
    vertexAttribArray vPosition $= Enabled
    return $ SquareData square
    where bufferOffset = Ptr.plusPtr Ptr.nullPtr . fromIntegral

  destroy Square = return ()

instance Drawable Square where
  draw (SquareData square) = do
    bindVertexArrayObject $= Just square
    drawArrays Triangles 0 6

