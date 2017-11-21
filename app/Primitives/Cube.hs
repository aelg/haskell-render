module Primitives.Cube
  ( Cube
  ) where

import           Drawable
import           Foreign.Marshal.Array
import qualified Foreign.Ptr               as Ptr
import qualified Foreign.Storable          as S
import           Graphics.Rendering.OpenGL
import           Initializable
import           Data.Word

data Cube =
  Cube VertexArrayObject NumArrayIndices
  deriving (Show)

instance Initializable Cube where
  create = do
    cube <- genObjectName -- VAO
    bindVertexArrayObject $= Just cube

    -- Add vertices
    let vertices = Vertex3 <$> [-1.0, 1.0] <*> [-1.0, 1.0] <*> [-1.0, 1.0] :: [Vertex3 GLfloat]
        numVertices = length vertices
        firstIndex = 0
        vPosition = AttribLocation 0
    print vertices
    arrayBuffer <- genObjectName -- Vertices
    bindBuffer ArrayBuffer $= Just arrayBuffer
    withArray vertices $ \ptr -> do
      let size = fromIntegral (numVertices * S.sizeOf (head vertices))
      bufferData ArrayBuffer $= (size, ptr, StaticDraw)
    vertexAttribPointer vPosition $=
      (ToFloat, VertexArrayDescriptor 3 Float 0 (bufferOffset firstIndex))
    vertexAttribArray vPosition $= Enabled

    -- Add indices
    let indices = [ 0, 1, 2
                  , 1, 3, 2
                  , 4, 1, 0
                  ] :: [GLuint]
        numIndices = length indices
    indicesBuffer <- genObjectName -- Indices
    bindBuffer ElementArrayBuffer $= Just indicesBuffer
    withArray indices $ \ptr -> do
      let size = fromIntegral (numIndices * S.sizeOf (head indices))
      bufferData ElementArrayBuffer $= (size, ptr, StaticDraw)

    return $ Cube cube $ fromIntegral numIndices
    where
      bufferOffset = Ptr.plusPtr Ptr.nullPtr . fromIntegral
  destroy _ = return ()

instance Drawable Cube where
  draw (Cube cube indices) = do
    bindVertexArrayObject $= Just cube
    drawElements Triangles indices UnsignedInt Ptr.nullPtr
