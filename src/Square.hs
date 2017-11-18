module Square
  ( Square
  ) where

import           Drawable
import           Foreign.Marshal.Array
import qualified Foreign.Ptr               as Ptr
import qualified Foreign.Storable          as S
import           Graphics.Rendering.OpenGL
import           Initializable

newtype Square =
  Square VertexArrayObject
  deriving (Show)

instance Initializable Square where
  create = do
    square <- genObjectName
    bindVertexArrayObject $= Just square
    let vertices =
          map
            (\(Vertex2 x y) -> Vertex3 (x * 0.8) (y * 0.8) (-2.0))
            [ Vertex2 1.0 (-1.0)
            , Vertex2 1.0 1.0
            , Vertex2 (-1.0) 1.0
            , Vertex2 1.0 (-1.0)
            , Vertex2 (-1.0) 1.0
            , Vertex2 (-1.0) (-1.0)
            ] :: [Vertex3 GLfloat]
        numVertices = length vertices
        firstIndex = 0
        vPosition = AttribLocation 0
    arrayBuffer <- genObjectName
    bindBuffer ArrayBuffer $= Just arrayBuffer
    withArray vertices $ \ptr -> do
      let size = fromIntegral (numVertices * S.sizeOf (head vertices))
      bufferData ArrayBuffer $= (size, ptr, StaticDraw)
    vertexAttribPointer vPosition $=
      (ToFloat, VertexArrayDescriptor 3 Float 0 (bufferOffset firstIndex))
    vertexAttribArray vPosition $= Enabled
    return $ Square square
    where
      bufferOffset = Ptr.plusPtr Ptr.nullPtr . fromIntegral
  destroy _ = return ()

instance Drawable Square where
  draw (Square square) = do
    bindVertexArrayObject $= Just square
    drawArrays Triangles 0 6
