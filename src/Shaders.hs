{-# LANGUAGE DeriveGeneric #-}

module Shaders
  ( Shaders
  , ShaderProgram(..)
  , activateProgram
  , getProgram
  ) where

import           Data.Hashable
import           Data.HashMap
import           GHC.Generics              (Generic)
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import           Initializable
import           ShaderLoader

newtype Shaders =
  Shaders (Map ShaderProgram GL.Program)

data ShaderProgram
  = Simple
  | SimpleFragment
  | Normal
  deriving (Generic, Eq, Ord)

instance Hashable ShaderProgram

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
  , ShaderInfo GL.FragmentShader simpleFragmentFile
  ]

simpleFragmentShader =
  [ ShaderInfo GL.VertexShader normalVertexFile
  , ShaderInfo GL.FragmentShader simpleFragmentFile
  ]

shaders =
  [ (Simple, simpleShader)
  , (Normal, normalShader)
  , (SimpleFragment, simpleFragmentShader)
  ]

activateProgram :: Shaders -> ShaderProgram -> IO GL.Program
activateProgram shaders shaderProgram = do
  let program = getProgram shaders shaderProgram
  GL.currentProgram $= Just program
  return program

getProgram :: Shaders -> ShaderProgram -> GL.Program
getProgram (Shaders programs) shaderProgram = programs ! shaderProgram

load (a, shader) = do
  program <- loadShaders shader
  return (a, program)

instance Initializable Shaders where
  create = do
    loadedShaders <- mapM load shaders
    let shaders = Shaders $ fromList loadedShaders
    activateProgram shaders Simple
    return shaders
  destroy _ = return ()
