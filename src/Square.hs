module Square (initSquare) where

import Descriptor
import Graphics.Rendering.OpenGL
import Foreign.Marshal.Array
import qualified Foreign.Storable as S

initSquare :: IO Descriptor
initSquare = do
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

  arrayBuffer <- genObjectName
  bindBuffer ArrayBuffer $= Just arrayBuffer
  withArray vertices $ \ptr -> do
    let size = fromIntegral (numVertices * S.sizeOf (head vertices))
    bufferData ArrayBuffer $= (size, ptr, StaticDraw)

  return $  Descriptor square 0 (fromIntegral numVertices)

