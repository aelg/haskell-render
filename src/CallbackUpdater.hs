module CallbackUpdater
  ( CallbackUpdater(..)
  ) where

import           Window

data CallbackUpdater = CallbackUpdater
  { updateWindowSize  :: WindowSizeCallback -> IO ()
  , updateWindowClose :: WindowCloseCallback -> IO ()
  }
