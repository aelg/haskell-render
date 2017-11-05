{-# LANGUAGE DeriveGeneric #-}
module Shaders (Shaders(Shaders)) where

import Initializable
import ShaderLoader
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Data.HashMap
import Data.Hashable
import GHC.Generics (Generic)

data Shaders = Shaders | ShadersData (Map ShaderProgram GL.Program)

data ShaderProgram = Simple 
                     deriving (Generic,Eq,Ord)
instance Hashable ShaderProgram

setProgram :: Shaders -> ShaderProgram -> IO ()
setProgram (ShadersData programs) shaderProgram = GL.currentProgram $= (Just $ programs ! shaderProgram)

instance Initializable Shaders where 
  create Shaders = do
    program <- loadShaders [ ShaderInfo GL.VertexShader (FileSource "shaders/simple_vertex_shader.glsl")
                           , ShaderInfo GL.FragmentShader (FileSource "shaders/simple_fragment_shader.glsl")
                           ]
    let shaders = ShadersData $ singleton Simple program
    setProgram shaders Simple
    return shaders

  destroy (ShadersData _) = return ()
