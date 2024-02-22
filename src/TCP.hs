{-# LANGUAGE LambdaCase #-}

module TCP where

import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Char8 as BS8
import Network.Socket
import Network.Socket.ByteString

import Codec
import Message
import Queue

------------------------------------------------------------------------

tcpSource :: Maybe HostName -> ServiceName -> Codec (Msg a) (Msg b) -> Queue (Msg a) -> IO ()
tcpSource mhost port c q = do
  addr <- resolve
  bracket (open addr) close loop
  where
    resolve = do
      let hints = defaultHints
            { addrFlags = [AI_PASSIVE]
            , addrSocketType = Stream
            }
      getAddrInfo (Just hints) mhost (Just port) >>=
        \case
          [] -> error "tcpSource: getAddrInfo failed"
          (i : _is) -> return i

    open addr = bracketOnError (openSocket addr) close $ \sock -> do
      setSocketOption sock ReuseAddr 1
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      listen sock 1024
      return sock

    loop sock = forever $ do
      bracketOnError (accept sock) (close . fst) $ \(conn, _peer) ->
        void $ forkIO $ withFdSocket conn $ \fd -> do
          threadWaitRead (fromIntegral fd)
          try (recv conn 1024) >>= \case
            Left err -> putStrLn (displayException (err :: IOError))
            Right bs -> case decode c bs of
              Left err' -> do
                print bs
                putStrLn (displayDecodeError err')
              Right msg -> writeQueue q (setMessageSocket msg conn)

tcpSink :: Codec (Msg a) (Msg b) -> Queue (Msg b) -> IO ()
tcpSink c q = forever $ do
  msg <- readQueue q
  case messageSocket msg of
    Nothing -> error "tcpSink: no socket to sink to"
    Just sock -> void $ forkIO $ withFdSocket sock $ \fd -> do
      threadWaitWrite (fromIntegral fd)
      sendAll sock (encode c msg) `finally` gracefulClose sock 5000

------------------------------------------------------------------------

ncDelay :: Show a => HostName -> Int -> Int -> Int -> Msg a -> IO ()
ncDelay host port0 sendDelayMs recvDelayMs req =
  runTCPClient (show port0) $ \s -> do
    threadDelay (sendDelayMs * 1000)
    sendAll s (BS8.pack (show req))
    threadDelay (recvDelayMs * 1000)
    resp <- recv s 1024
    BS8.putStrLn resp
  where
    runTCPClient :: ServiceName -> (Socket -> IO a) -> IO a
    runTCPClient port client = withSocketsDo $ do
        addr <- resolve
        bracket (open addr) close client
      where
        resolve = do
          let hints = defaultHints { addrSocketType = Stream }
          getAddrInfo (Just hints) (Just host) (Just port) >>=
            \case
              [] -> error "runTCPClient: getAddrInfo failed"
              (i : _is) -> return i
        open addr = bracketOnError (openSocket addr) close $ \sock -> do
          connect sock $ addrAddress addr
          return sock

nc :: Show a => HostName -> Int -> Msg a -> IO ()
nc host port req = ncDelay host port 0 0 req
