module Initializable where 

import Descriptor

class Initializable a where
  create :: a -> IO a
  destroy :: a -> IO ()

