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
import Syntax.StateMachine.Typed
import Interpreter

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
          => Name -> T s' i o -> Maybe (s -> s') -> Msg a
  UpgradeSucceeded :: Name -> Msg a
  UpgradeFailed :: Name -> Msg a

  UpgradePipeline_ :: Ty_ -> Ty_ -> UP -> Msg a
  UpgradePipeline :: P String String -> Msg String

  Done :: Msg a

instance Show a => Show (Msg a) where
  show (Item x) = "Item " ++ show x
  show (Upgrade name _sm _g) = "Upgrade " ++ name
  show (UpgradeSucceeded name) = "UpgradeSucceeded " ++ name
  show (UpgradeFailed name) = "UpgradeFailed " ++ name
  show (UpgradePipeline_ ua ub up) = "UpgradePipeline_ " ++ show ua ++ " " ++ show ub ++ " " ++ show up
  show (UpgradePipeline {}) = "UpgradePipeline {}"
  show Done = "Done"

instance Read a => Read (Msg a) where
  readPrec = itemP <|> upgradePipelineP
    where
      itemP = do
        Ident "Item" <- lexP
        Item <$> readPrec
      upgradePipelineP = do
        Ident "UpgradePipeline_" <- lexP
        UpgradePipeline_ <$> readPrec <*> readPrec <*> parens readPrec

deploy :: forall a b. (Typeable a, Typeable b)
       => P a b -> Queue (Msg a) -> IO (Queue (Msg b))
deploy IdP             q = return q
deploy (f :>>> g)      q = deploy g =<< deploy f q
deploy (SM name s0 f0) q = do
  q' <- newQueue
  let go :: Typeable s => s -> T s a b -> IO ()
      go s f = do
        m <- readQueue q
        case m of
          Item i  -> do
            let (o, s') = runT f i s
            writeQueue q' (Item o)
            go s' f
          Upgrade name' f' mg
            | name /= name' -> do
                writeQueue q' (Upgrade name' f' mg)
                go s f
            | otherwise -> case (cast f', cast s) of
                             (Just (f'' :: T s a b), Just s') -> do
                               writeQueue q' (UpgradeSucceeded name)
                               case mg of
                                 Nothing -> go s f''
                                 Just g  -> go (g s') undefined
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
          UpgradePipeline {} -> error "deploy, impossible, handled upsteam in `run`"
          UpgradePipeline_ {} -> error "deploy, impossible, handled upsteam in `run`"
  _pid <- forkIO (go s0 f0)
  return q'

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
run (FromList xs0) p StdOut = do
  q <- newQueue
  q' <- deploy p q
  let go [] = return ()
      go (x : xs) = do
        case x of
          UpgradePipeline p' -> do
            putStrLn "<Upgraded pipeline>"
            run (FromList xs) p' StdOut
          _otherwise -> do
            writeQueue q x
            y <- readQueue q'
            print y
            go xs
  go xs0
run StdIn p StdOut = do
  q <- newQueue
  q' <- deploy p q
  let go = do
        l <- getLine
        case readMaybe l of
          Just (UpgradePipeline_ ua' ub' up') -> do
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
