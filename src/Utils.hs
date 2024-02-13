module Utils where

import Control.Exception
import Control.Concurrent

------------------------------------------------------------------------

withForkIO_ :: IO () -> IO a -> IO a
withForkIO_ setup m = bracket (forkIO setup) killThread (\_ -> m)
