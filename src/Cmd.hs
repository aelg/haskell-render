module Cmd (Cmd(..)) where

data (Show a) =>
     Cmd a = SpaceBar a 
     | Shutdown
     | NoCmd deriving Show

