{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Deployment where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Typeable
import Text.Read

import Syntax.Types
import TypeCheck.Pipeline
import Syntax.Pipeline.Untyped
import Syntax.Pipeline.Typed

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
  Upgrade :: (Typeable s, Typeable s', Typeable i, Typeable o)
          => Name -> SM s' i o -> (s -> s') -> Msg a
  UpgradeSucceeded :: Name -> Msg a
  UpgradeFailed :: Name -> Msg a

  UpgradePipeline :: Ty_ -> Ty_ -> UP -> Msg a

  Done :: Msg a

instance Show a => Show (Msg a) where
  show (Item x) = show x
  show (Upgrade name _sm _g) = "Upgrade " ++ name
  show (UpgradeSucceeded name) = "UpgradeSucceeded " ++ name
  show (UpgradeFailed name) = "UpgradeFailed " ++ name
  show (UpgradePipeline ua ub up) = "UpgradePipeline " ++ show ua ++ " " ++ show ub ++ " " ++ show up
  show Done = "Done"

instance Read a => Read (Msg a) where
  readPrec = itemP <|> upgradePipelineP
    where
      itemP = do
        Ident "Item" <- lexP
        Item <$> readPrec
      upgradePipelineP = do
        Ident "UpgradePipeline" <- lexP
        UpgradePipeline <$> readPrec <*> readPrec <*> readPrec

deploy :: forall a b. (Typeable a, Typeable b)
       => P a b -> Queue (Msg a) -> IO (Queue (Msg b))
deploy IdP             q = return q
deploy (f :>>> g)      q = deploy g =<< deploy f q
deploy (SM name s0 f0) q = do
  q' <- newQueue
  let go :: Typeable s => s -> SM s a b -> IO ()
      go s f = do
        m <- readQueue q
        case m of
          Item i  -> do
            let (s', o) = f i s
            writeQueue q' (Item o)
            go s' f
          Upgrade name' f' g
            | name /= name' -> do
                writeQueue q' (Upgrade name' f' g)
                go s f
            | otherwise -> case (cast f', cast s) of
                             (Just f'', Just s') -> do
                               writeQueue q' (UpgradeSucceeded name)
                               go (g s') f''
                             _ -> do
                               writeQueue q' (UpgradeFailed name)
                               go s f
          UpgradeSucceeded name' -> do
            writeQueue q' (UpgradeSucceeded name')
            go s f
          UpgradeFailed name' -> do
            writeQueue q' (UpgradeFailed name')
            go s f
          Done -> do
            writeQueue q' Done
            return ()
          UpgradePipeline {} -> error "deploy, impossible, handled upsteam"
  _pid <- forkIO (go s0 f0)
  return q'

data Input = ReadCount | IncrCount
  deriving Read
data Output = Count Int | Ok
  deriving Show

counter :: Input  -> Int -> (Int, Output)
counter ReadCount n = (n, Count n)
counter IncrCount n = (n + 1, Ok)

counter2 :: Input  -> (Int, Int) -> ((Int, Int), Output)
counter2 ReadCount (old, new) = ((old, new), Count new)
counter2 IncrCount (old, new) = ((old, new + 2), Ok)

------------------------------------------------------------------------

data Source a where
  FromList :: [Msg a] -> Source a
  StdIn :: Read a => Source a
  -- FromFile :: FilePath -> Source Text

data Sink b r where
  ToList :: Sink b [Msg b]
  StdOut :: Show b => Sink b ()
  -- ToFile :: FilePath -> Source Text ()

run :: (Typeable a, Typeable b) => Source a -> P a b -> Sink b r -> IO r
run (FromList xs) p ToList = do
  q <- newQueue
  q' <- deploy p q
  mapM_ (writeQueue q) xs
  replicateM (length xs) (readQueue q')
run (FromList xs) p StdOut = do
  q <- newQueue
  mapM_ (writeQueue q) xs
  q' <- deploy p q
  ys <- replicateM (length xs) (readQueue q')
  mapM_ print ys
run StdIn p StdOut = do
  q <- newQueue
  q' <- deploy p q
  let go = do
        l <- getLine
        case readMaybe l of
          Just (UpgradePipeline ua' ub' up') -> do
            case (inferTy ua', inferTy ub') of
              (ETy a', ETy b') ->
                case typeCheckP a' b' up' of
                  Just (EP Witness Witness p') -> do
                    putStrLn "<Upgraded pipeline>"
                    run StdIn p' StdOut
                  Nothing -> putStrLn "<Upgrade failed>" >> go
          Just msg -> do
            writeQueue q msg
            y <- readQueue q'
            print y
            go
          Nothing -> putStrLn "<Parse error>" >> go
  go
run StdIn _p ToList = undefined

------------------------------------------------------------------------

test :: IO [Msg Output]
test = run (FromList xs) (SM "counter" 0 counter) ToList
  where
    xs =
      [ Item ReadCount
      , Item IncrCount
      , Item IncrCount
      , Item ReadCount
      , Upgrade "counter" counter2 (\n -> (n, n))
      , Item ReadCount
      , Item IncrCount
      , Item ReadCount
      ]
-- >>> test
-- [Count 0,Ok,Ok,Count 2,UpgradeSucceeded counter,Count 2,Ok,Count 4]

------------------------------------------------------------------------

data D a b = D
  { dInput  :: Queue (Msg a)
  , dOutput :: Queue (Msg b)
  }

redeploy :: (Typeable a', Typeable b') => P a' b' -> D a b -> IO (D a' b')
redeploy p (D q _q') = do
  writeQueue q Done
  q'' <- newQueue
  q''' <- deploy p q''
  return (D q'' q''')


  {-
appendDeploy :: P b c -> Deployment a b -> IO (Deployment a c)
appendDeploy = undefined

prependDeploy :: P a b -> Deployment b c -> IO (Deployment a c)
prependDeploy = undefined

upgrade :: Name -> T x y -> Deployment a b -> IO (Deployment a b)
upgrade = undefined

extend :: P a c -> Deployment b c -> Deployment (Either a b) c

-}
