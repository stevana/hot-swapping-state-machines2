{-# LANGUAGE LambdaCase #-}

module TCP where

import Control.Exception
import Control.Monad
import Network.Socket
import Network.Socket.ByteString
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8

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

    loop sock = forever $
      bracketOnError (accept sock) (close . fst) $ \(conn, _peer) -> do
        try (recv conn 1024) >>= \case
          Left err -> putStrLn (displayException (err :: IOError))
          Right bs -> case decode c bs of
            Left err' -> putStrLn (displayDecodeError err')
            Right msg -> writeQueue q (setMessageSocket msg conn)

tcpSink :: Codec (Msg a) (Msg b) -> Queue (Msg b) -> IO ()
tcpSink c q = forever $ do
  msg <- readQueue q
  case messageSocket msg of
    Nothing -> error "tcpSink: no socket to sink to"
    Just sock -> sendAll sock (encode c msg) `finally` gracefulClose sock 5000

------------------------------------------------------------------------

nc :: Show a => HostName -> Int -> Msg a -> IO ()
nc host port0 req =
  runTCPClient (show port0) $ \s -> do
    sendAll s (BS8.pack (show req))
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
