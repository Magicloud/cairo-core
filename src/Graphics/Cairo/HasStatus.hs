module Graphics.Cairo.HasStatus where

import Control.Exception
import Control.Monad
import Graphics.Cairo.Types
import Graphics.Cairo.Utilities.ErrorHandling

class HasStatus a where
  status :: a -> IO Status
  use :: a -> (a -> IO b) -> IO b
  use a = bracket (return a) (failStatus <=< status)
