module Cmd
  ( Cmd(..)
  ) where

import           Keyboard

data Cmd a
  = KeyPress [(KeyAction, a)]
  | Shutdown
  | Redraw
  | Print String
  | RunIO (IO a)
