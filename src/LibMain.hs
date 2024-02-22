module LibMain where

import Control.Exception
import Control.Concurrent
import System.Directory

import Deployment
import Example.Counter
import Syntax.Pipeline.Typed
import Codec
import Upgrade
import TCP
import Message
import Syntax.StateMachine.Untyped
import Syntax.Types

------------------------------------------------------------------------

libMain :: IO ()
libMain = do
  putStrLn "Starting TCP server on 127.0.0.1:3000"
  putStr "Capabilities: "
  print =<< getNumCapabilities
  run (FromTCP "127.0.0.1" 3000) readShowCodec (SM "counter" 0 counterV1) ToTCP

-- XXX: Fix "Exception: Network.Socket.connect: <socket: 13>: does not exist
-- (Connection refused)" when test is rerun. This is because we are sending V1
-- requests to V2 server...
test :: IO ()
test = do
  nc "127.0.0.1" 3000 (Item_ (show ReadCountV1))
  nc "127.0.0.1" 3000 (Item_ (show IncrCountV1))
  nc "127.0.0.1" 3000 (Item_ (show IncrCountV1))
  nc "127.0.0.1" 3000 (Item_ (show ReadCountV1))

  let msg :: Msg ()
      msg = Upgrade_ "counter"
              (UpgradeData_ UTInt UTInt UTString UTString counterV2U IdU)
  nc "127.0.0.1" 3000 msg

  nc "127.0.0.1" 3000 (Item_ (show ReadCountV2))
  nc "127.0.0.1" 3000 (Item_ (show ResetCountV2))
  nc "127.0.0.1" 3000 (Item_ (show ReadCountV2))

delayTest :: IO ()
delayTest = do
  let port = 3000
  mapM_ forkIO
    [ ncDelay "127.0.0.1" port 0 3000 (Item_ (show ReadCountV1))
    , ncDelay "127.0.0.1" port 0    0 (Item_ (show IncrCountV1))
    , ncDelay "127.0.0.1" port 0    0 (Item_ (show IncrCountV1))
    , ncDelay "127.0.0.1" port 0    0 (Item_ (show ReadCountV1))
    ]

-- XXX: fix "withFile: resource busy (file is locked)" error.
testFile :: IO ()
testFile = do
  let fp = "/tmp/test-file-source-and-sink.txt"
  bracket_ (writeFile fp "apa bepa\ncepa depa epa\nfepa") (removeFile fp) $ do
    run (FromFile fp) idCodec IdP (ToFile fp)
    putStrLn =<< readFile fp
