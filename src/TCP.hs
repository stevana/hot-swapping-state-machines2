{-# LANGUAGE LambdaCase #-}

module TCP where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.ByteString
import Network.Socket
import Network.Socket.ByteString

import Codec
import Message
import Queue

------------------------------------------------------------------------

tcpSource :: Maybe HostName -> ServiceName -> Queue Socket -> IO ()
tcpSource mhost port q = withSocketsDo $ do
  addr <- resolve
  bracket (open addr) close loop
  where
    resolve = do
      let hints = defaultHints
            { addrFlags = [AI_PASSIVE]
            , addrSocketType = Stream
            }
      infos <- getAddrInfo (Just hints) mhost (Just port)
      case infos of
        [] -> error "tcpSource: getAddrInfo failed"
        (i : _is) -> return i

    open addr = bracketOnError (openSocket addr) close $ \sock -> do
      setSocketOption sock ReuseAddr 1
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      listen sock 1024
      return sock

    loop sock = forever $
      bracketOnError (accept sock) (close . fst) $ \(conn, _peer) ->
        writeQueue q conn

tcpSink :: Queue (Socket, ByteString) -> IO ()
tcpSink q = forever $ do
  (sock, resp) <- readQueue q
  sendAll sock resp `finally` gracefulClose sock 5000

------------------------------------------------------------------------

tcpRead :: Queue Socket -> Queue (Socket, ByteString) -> IO ()
tcpRead q q' = forever $ do
  conn <- readQueue q
  try (recv conn 1024) >>= \case
    Left err -> putStrLn (displayException (err :: IOError))
    Right bs -> writeQueue q' (conn, bs)

testTcp :: IO ()
testTcp = do
  q <- newQueue
  q' <- newQueue
  bracket (forkIO (tcpSource Nothing "3000" q)) killThread $ \_ ->
    bracket (forkIO (tcpRead q q')) killThread $ \_ -> do
      tcpSink q'

------------------------------------------------------------------------

tcpSource' :: Maybe HostName -> ServiceName -> Codec (Msg a) (Msg b) -> Queue (Msg a) -> IO ()
tcpSource' mhost port c q = do
  addr <- resolve
  bracket (open addr) close loop
  where
    resolve = do
      let hints = defaultHints
            { addrFlags = [AI_PASSIVE]
            , addrSocketType = Stream
            }
      infos <- getAddrInfo (Just hints) mhost (Just port)
      case infos of
        [] -> error "tcpSource: getAddrInfo failed"
        (i : _is) -> return i

    open addr = bracketOnError (openSocket addr) close $ \sock -> do
      setSocketOption sock ReuseAddr 1
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      listen sock 1024
      return sock

    loop sock = forever $
      bracketOnError (accept sock) (close . fst) $ \(conn, _peer) -> do
        try (recv conn 1024) >>= \case
          Left err -> putStrLn (displayException (err :: IOError))
          Right bs -> case decode c bs of
            Left err' -> putStrLn (displayDecodeError err')
            Right msg -> writeQueue q (setMessageSocket msg conn)

tcpSink' :: Codec (Msg a) (Msg b) -> Queue (Msg b) -> IO ()
tcpSink' c q = forever $ do
  msg <- readQueue q
  case messageSocket msg of
    Nothing -> error "tcpSink'"
    Just sock -> sendAll sock (encode c msg) `finally` gracefulClose sock 5000
