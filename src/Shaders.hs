{-# LANGUAGE DeriveGeneric #-}

module Shaders
  ( Shaders(Shaders)
  ) where

import           Data.Hashable
import           Data.HashMap
import           GHC.Generics              (Generic)
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import           Initializable
import           ShaderLoader

data Shaders
  = Shaders
  | ShadersData (Map ShaderProgram GL.Program)

data ShaderProgram
  = Simple
  | Normal
  deriving (Generic, Eq, Ord)

instance Hashable ShaderProgram

setProgram :: Shaders -> ShaderProgram -> IO ()
setProgram (ShadersData programs) shaderProgram =
  GL.currentProgram $= (Just $ programs ! shaderProgram)

simpleVertexFile = FileSource "shaders/simple_vertex_shader.glsl"

simpleFragmentFile = FileSource "shaders/simple_fragment_shader.glsl"

simpleShader =
  [ ShaderInfo GL.VertexShader simpleVertexFile
  , ShaderInfo GL.FragmentShader simpleFragmentFile
  ]

normalVertexFile = FileSource "shaders/vertex_shader.glsl"

normalFragmentFile = FileSource "shaders/fragment_shader.glsl"

normalShader =
  [ ShaderInfo GL.VertexShader normalVertexFile
  , ShaderInfo GL.FragmentShader normalFragmentFile
  ]

shaders = [(Simple, simpleShader), (Normal, normalShader)]

load (a, shader) = do
  program <- loadShaders shader
  return (a, program)

instance Initializable Shaders where
  create Shaders = do
    loadedShaders <- mapM load shaders
    let shaders = ShadersData $ fromList loadedShaders
    setProgram shaders Simple
    return shaders
  destroy (ShadersData _) = return ()
