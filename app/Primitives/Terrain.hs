module Primitives.Terrain
  ( Terrain
  ) where

import           Data.Word
import           Drawable
import           Foreign.Marshal.Array
import qualified Foreign.Ptr               as Ptr
import qualified Foreign.Storable          as S
import           GHC.Float
import           Graphics.Rendering.OpenGL

import           Initializable
import           Matrix
import           Rotation

data Terrain =
  Terrain Int
          VertexArrayObject
          NumArrayIndices
  deriving (Show)

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

rgbToV :: Int -> Int -> Int -> Vertex3 GLfloat
rgbToV r g b =
  Vertex3 (fromIntegral r / 255) (fromIntegral g / 255) (fromIntegral b / 255)

fromCenter x z = sqrt ((0.5-x)**2 + (0.5-z)**2)

sigmoid x = 1 / exp (1/max 0.001 (1-x*2)-1)

heightFunc (x, z) = (0.01 * sin (x * 10 * 2 * pi) * cos (z * 10 * 2 * pi)) * sigmoid (fromCenter x z)

normalF :: (Float, Float) -> Vertex3 GLfloat
normalF (x, z) = Vertex3 nx ny nz
  where
    e = 0.0001
    dx = (heightFunc (x + e, z) - heightFunc (x - e, z)) / (2 * e)
    dz = (heightFunc (x, z + e) - heightFunc (x, z - e)) / (2 * e)
    tx = rotateV xHat (mkRotation (float2Double $ atan dx) zHat)
    tz = rotateV zHat (mkRotation (float2Double $ -atan dz) xHat)
    [nx, ny, nz] = vector3ToFloatList $ cross tz tx

instance Initializable Terrain where
  create = do
    terrain <- genObjectName -- VAO
    bindVertexArrayObject $= Just terrain
    let gridCount = 200 :: GLuint
        gridSize = 10000
        grid =
          map floatGrid $
          (,) <$> [0 .. (gridCount - 1)] <*> [0 .. (gridCount - 1)] :: [( Float
                                                                      , Float)]
        floatGrid (x, z) =
          ( fromIntegral x / fromIntegral gridCount
          , fromIntegral z / fromIntegral gridCount)
    -- Add vertices
    let vertices =
          map
            (\(x, z) ->
               Vertex3
                 (gridSize * (x - 0.5))
                 (gridSize * heightFunc (x, z) - 1)
                 (gridSize * (z - 0.5)))
            grid :: [Vertex3 GLfloat]
    addAttribArray vertices 0
    -- Add color
    let color = map (const (rgbToV 219 184 114)) grid
    addAttribArray color 1
    -- Add normals
    let normals = map normalF grid :: [Vertex3 GLfloat]
    addAttribArray normals 2
    -- Add indices
    let indices = concatMap f [0 .. (gridCount - 2)] :: [GLuint]
        f strip =
          concat $
          zipWith
            (\x y -> [x, y] :: [GLuint])
            [(strip + 1) * gridCount .. ((strip + 2) * gridCount) - 1]
            [strip * gridCount .. ((strip + 1) * gridCount) - 1]
        numIndices = length indices
    indicesBuffer <- genObjectName -- Indices
    bindBuffer ElementArrayBuffer $= Just indicesBuffer
    withArray indices $ \ptr -> do
      let size = fromIntegral (numIndices * S.sizeOf (head indices))
      bufferData ElementArrayBuffer $= (size, ptr, StaticDraw)
    return $ Terrain (fromIntegral gridCount) terrain $ fromIntegral numIndices
    where
      bufferOffset = Ptr.plusPtr Ptr.nullPtr . fromIntegral
  destroy _ = return ()

instance Drawable Terrain where
  draw (Terrain gridCount terrain indices) = do
    bindVertexArrayObject $= Just terrain
    mapM_ f [0 .. (gridCount - 2)]
    where
      f strip =
        drawElements
          TriangleStrip
          (fromIntegral gridCount * 2)
          UnsignedInt
          (stripStart strip)
      stripStart strip =
        Ptr.plusPtr Ptr.nullPtr (4 * fromIntegral gridCount * 2 * strip)
