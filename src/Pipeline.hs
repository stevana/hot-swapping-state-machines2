{-# LANGUAGE GADTs #-}

module Pipeline where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Data.Typeable

------------------------------------------------------------------------

type SM s i o = i -> s -> (s, o)

data P a b where
  IdP    :: P a a
  (:>>>) :: Typeable b => P a b -> P b c -> P a c
  SM     :: Typeable s => Name -> Initial s -> SM s a b -> P a b

type Initial s = s
type Name = String

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

data Msg a where
  Item    :: a -> Msg a
  Upgrade :: (Typeable s, Typeable i, Typeable o) => Name -> SM s i o -> Msg a
  UpgradeSucceeded :: Name -> Msg a
  UpgradeFailed :: Name -> Msg a

instance Show a => Show (Msg a) where
  show (Item x) = show x
  show (Upgrade name _sm) = "Upgrade " ++ name
  show (UpgradeSucceeded name) = "UpgradeSucceeded " ++ name
  show (UpgradeFailed name) = "UpgradeFailed " ++ name

deploy :: (Typeable a, Typeable b) => P a b -> TBQueue (Msg a) -> IO (TBQueue (Msg b))
deploy IdP             q = return q
deploy (f :>>> g)      q = deploy g =<< deploy f q
deploy (SM name s0 f0) q = do
  q' <- newQueue
  let go s f = do
        m <- readQueue q
        case m of
          Item i  -> do
            let (s', o) = f i s
            writeQueue q' (Item o)
            go s' f
          Upgrade name' f'
            | name /= name' -> do
                writeQueue q' (Upgrade name' f')
                go s f
            | otherwise -> case cast f' of
                             Just f'' -> do
                               writeQueue q' (UpgradeSucceeded name)
                               go s f''
                             Nothing -> do
                               writeQueue q' (UpgradeFailed name)
                               go s f
          UpgradeSucceeded name' -> do
            writeQueue q' (UpgradeSucceeded name')
            go s f
          UpgradeFailed name' -> do
            writeQueue q' (UpgradeFailed name')
            go s f
  _pid <- forkIO (go s0 f0)
  return q'

data Input = ReadCount | IncrCount
data Output = Count Int | Ok
  deriving Show

counter :: Input  -> Int -> (Int, Output)
counter ReadCount n = (n, Count n)
counter IncrCount n = (n + 1, Ok)

counter2 :: Input  -> Int -> (Int, Output)
counter2 ReadCount n = (n, Count n)
counter2 IncrCount n = (n + 2, Ok)

test :: IO ()
test = do
  q <- newQueue
  q' <- deploy (SM "counter" 0 counter) q
  let xs = [ Item ReadCount
           , Item IncrCount
           , Item IncrCount
           , Item ReadCount
           , Upgrade "counter" counter2
           , Item ReadCount
           , Item IncrCount
           , Item ReadCount
           ]
  mapM_ (writeQueue q) xs
  print =<< replicateM (length xs) (readQueue q')

-- >>> test
-- [Count 0,Ok,Ok,Count 2,UpgradeSucceeded counter,Count 2,Ok,Count 4]

------------------------------------------------------------------------

  {-
data Deployment a b = Deployment
  { dInput  :: Queue a
  , dOutput :: Queue b
  }

redeploy :: P a b -> Deployment a b -> IO (Deployment a b)
redeploy = undefined

appendDeploy :: P b c -> Deployment a b -> IO (Deployment a c)
appendDeploy = undefined

prependDeploy :: P a b -> Deployment b c -> IO (Deployment a c)
prependDeploy = undefined

upgrade :: Name -> T x y -> Deployment a b -> IO (Deployment a b)
upgrade = undefined

-}
