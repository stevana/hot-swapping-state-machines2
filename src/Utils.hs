module Utils where

import Control.Exception
import Control.Concurrent

------------------------------------------------------------------------

withForkIO_ :: IO () -> IO () -> IO ()
withForkIO_ setup m = bracket (forkIO setup) killThread (\_ -> m)
