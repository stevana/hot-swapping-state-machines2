{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Deployment where

import Control.Concurrent
import qualified Data.ByteString.Char8 as BS8
import System.IO
import Data.Typeable

import Codec
import Interpreter
import Message
import Queue
import Syntax.Pipeline.Typed
import Syntax.StateMachine.Typed
import TCP
import Utils
import Upgrade

------------------------------------------------------------------------

data Source a
  = Stdin
  | FromFile FilePath
  | FromTCP String Int
  | FromList [Msg a]

data Sink b r where
  Stdout :: Sink b ()
  ToFile :: FilePath -> Sink b ()
  ToTCP  :: Sink b ()
  ToList :: Sink b [Msg b]

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
  withFile fp ReadMode $ \h -> do
    hSetBuffering h LineBuffering
    loop h
  where
    loop h = do
      eof <- hIsEOF h
      if eof
      then writeQueue q Done
      else do
        s <- BS8.hGetLine h
        case decode c s of
          Left err  -> do
            putStrLn (displayDecodeError err)
            loop h
          Right msg -> do
            writeQueue q msg
            loop h
source (FromTCP host port) c q =
  tcpSource (Just host) (show port) c q

source (FromList msgs) _c q = do
  mapM_ (writeQueue q) msgs
  writeQueue q Done

sink :: Sink b r -> Codec (Msg a) (Msg b) -> Queue (Msg b) -> IO r
sink Stdout c q = loop
  where
    loop = do
      msg <- readQueue q
      case msg of
        Done       -> return ()
        _otherwise -> BS8.putStrLn (encode c msg) >> loop
sink ToTCP c q = tcpSink c q
sink (ToFile fp) c q =
  withFile fp WriteMode $ \h -> do
    hSetBuffering h LineBuffering
    loop h
  where
    loop h = do
      msg <- readQueue q
      case msg of
        Done -> return ()
        _otherwise -> BS8.hPutStrLn h (encode c msg) >> loop h
sink ToList _c q = flushQueue q

------------------------------------------------------------------------

run :: (Typeable a, Typeable b) => Source a -> Codec (Msg a) (Msg b) -> P a b -> Sink b r -> IO r
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
          Upgrade msock name' ud
            | name /= name' -> do
                writeQueue q' (Upgrade msock name' ud)
                go s f
            | otherwise ->
                case typeCheckUpgrade s f ud of
                  Right (UpgradeData (f' :: T s a b) (g :: T () s s)) -> do
                    writeQueue q' (UpgradeSucceeded msock name)
                    let (s', ()) = runT g s ()
                    go s' f'
                  Left err -> do
                    print ud
                    print err
                    writeQueue q' (UpgradeFailed msock name)
                    go s f
          UpgradeSucceeded msock name' -> do
            writeQueue q' (UpgradeSucceeded msock name')
            go s f
          UpgradeFailed msock name' -> do
            writeQueue q' (UpgradeFailed msock name')
            go s f
          Done -> writeQueue q' Done
  -- The this process will terminate when `Done` is processed.
  _pid <- forkIO (go s0 f0)
  return q'
