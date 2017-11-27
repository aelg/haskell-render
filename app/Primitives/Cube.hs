module Primitives.Cube
  ( Cube
  ) where

import           Data.Word
import           Drawable
import           Foreign.Marshal.Array
import qualified Foreign.Ptr               as Ptr
import qualified Foreign.Storable          as S
import           Graphics.Rendering.OpenGL
import           Initializable

data Cube =
  Cube VertexArrayObject
       NumArrayIndices
  deriving (Show)

--normals vertices indicies =
{-
     2+---------+6
     /|        /|
    / |       / |    y
  3+---------+7 |    |
   |  |      |  |    +--- x
   | 0+------|-+4   /
   | /       | /   z
   |/        |/
  1+---------+5

-}
addAttribArray xs location = do
  let num = length xs
      firstIndex = 0
      position = AttribLocation location
  buffer <- genObjectName
  bindBuffer ArrayBuffer $= Just buffer
  withArray xs $ \ptr -> do
    let size = fromIntegral (num * S.sizeOf (head xs))
    bufferData ArrayBuffer $= (size, ptr, StaticDraw)
  vertexAttribPointer position $=
    (ToFloat, VertexArrayDescriptor 3 Float 0 (bufferOffset firstIndex))
  vertexAttribArray position $= Enabled
  where
    bufferOffset = Ptr.plusPtr Ptr.nullPtr . fromIntegral

instance Initializable Cube where
  create = do
    cube <- genObjectName -- VAO
    bindVertexArrayObject $= Just cube
    -- Add vertices
    let vertices =
          concat $
          replicate 3 $ Vertex3 <$> [-1.0, 1.0] <*> [-1.0, 1.0] <*> [-1.0, 1.0] :: [Vertex3 GLfloat]
    addAttribArray vertices 0
    -- Add color
    let color =
          concat $
          replicate 3 $ Vertex3 <$> [-1.0, 1.0] <*> [-1.0, 1.0] <*> [-1.0, 1.0] :: [Vertex3 GLfloat]
    addAttribArray color 1
    -- Add normals
    let normals =
          concat
            [ replicate 4 $ Vertex3 (-1.0) 0.0 0.0
            , replicate 4 $ Vertex3 1.0 0.0 0.0
            , replicate 4 $ Vertex3 0.0 0.0 1.0
            , replicate 4 $ Vertex3 0.0 0.0 (-1.0)
            , replicate 4 $ Vertex3 0.0 (-1.0) 0.0
            , replicate 4 $ Vertex3 0.0 1.0 0.0
            ] :: [Vertex3 GLfloat]
    addAttribArray normals 2
    -- Add indices
    let indices =
          concat
            [ [0, 1, 2]
            , [1, 3, 2]
            , [4, 6, 5]
            , [5, 6, 7]
            , [9, 15, 11] -- [1, 7, 3]
            , [9, 13, 15] -- [1, 5, 7]
            , [8, 10, 14] -- [0, 2, 6]
            , [8, 14, 12] -- [0, 6, 4]
            , [16, 17, 20]
            , [17, 5, 20]
            , [18, 22, 23]
            , [18, 23, 19]
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
