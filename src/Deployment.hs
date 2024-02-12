{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Deployment where

import Control.Concurrent
import Control.Monad
import Data.Typeable
import qualified Data.ByteString.Char8 as BS8
import Network.Socket
import Text.Read
import System.IO

import Interpreter
import Queue
import Syntax.Pipeline.Typed
import Syntax.StateMachine.Typed
import Syntax.Types
import TypeCheck.Pipeline
import TCP
import Utils
import Codec
import Message

------------------------------------------------------------------------

data Source a
  = Stdin
  | FromFile FilePath
  | FromTCP Int
  | FromList [Msg a]

data Sink
  = Stdout
  | ToFile FilePath
  | ToTCP

------------------------------------------------------------------------

source :: Source a -> Codec (Msg a) (Msg b) -> Queue (Msg a) -> IO ()
source Stdin c q = loop
  where
    loop = do
      s <- BS8.getLine
      case s of
        "\EOT" -> writeQueue q Done
        _otherwise -> case decode c s of
                        Left err  -> do
                          putStrLn (displayDecodeError err)
                          loop
                        Right msg -> do
                          writeQueue q msg
                          loop
source (FromFile fp) c q =
  withFile fp ReadMode $ \h ->
    loop h
  where
    loop h = do
      s <- BS8.hGetLine h
      case decode c s of
        Left err  -> do
          putStrLn (displayDecodeError err)
          loop h
        Right msg -> do
          writeQueue q msg
          loop h
source (FromTCP port) c q =
  tcpSource Nothing (show port) c q

source (FromList msgs) _c q = do
  mapM_ (writeQueue q) msgs
  writeQueue q Done

sink :: Sink -> Codec (Msg a) (Msg b) -> Queue (Msg b) -> IO ()
sink Stdout c q = loop
  where
    loop = do
      msg <- readQueue q
      case msg of
        Done       -> return ()
        _otherwise -> BS8.putStrLn (encode c msg) >> loop
sink ToTCP c q = tcpSink c q

run :: (Typeable a, Typeable b) => Source a -> Codec (Msg a) (Msg b) -> P a b -> Sink -> IO ()
run src codec p snk = do
  q <- newQueue
  withForkIO_ (source src codec q) $ do
    q' <- deploy p q
    sink snk codec q'

------------------------------------------------------------------------

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
          Item msock i -> do
            let (o, s') = runT f i s
            writeQueue q' (Item msock o)
            go s' f
          Upgrade msock name' f' mg
            | name /= name' -> do
                writeQueue q' (Upgrade msock name' f' mg)
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
  _pid <- forkIO (go s0 f0)
  return q'

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
