module Descriptor (Descriptor(..)) where

import Graphics.Rendering.OpenGL

data Descriptor = Descriptor VertexArrayObject ArrayIndex NumArrayIndices
