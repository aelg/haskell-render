module Cmd (Cmd(..)) where

data Cmd a = SpaceBar a 
     | Shutdown
     | Redraw
     | Print String
     | NoCmd

