module Queue where

import Control.Exception
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

------------------------------------------------------------------------

type Queue a = TBQueue a

newQueue :: IO (Queue a)
newQueue = newTBQueueIO mAX_QUEUE_LENGTH
  where
    mAX_QUEUE_LENGTH = 128

readQueue :: Queue a -> IO a
readQueue = atomically . readTBQueue

writeQueue :: Queue a -> a -> IO ()
writeQueue q x = atomically (writeTBQueue q x)

------------------------------------------------------------------------

withQueue :: Show a => (Queue a -> IO ()) -> IO ()
withQueue k = do
  q <- newQueue
  bracket (forkIO (k q)) killThread $ \_pid ->
    forever $ do
      x <- readQueue q
      print x
