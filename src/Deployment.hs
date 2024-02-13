{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Deployment where

import Control.Concurrent
import qualified Data.ByteString.Char8 as BS8
import Data.Typeable
import System.IO

import Codec
import Interpreter
import Message
import Queue
import Syntax.Pipeline.Typed
import Syntax.StateMachine.Typed
import Syntax.StateMachine.Untyped
import Syntax.Types
import TCP
import TypeCheck.Pipeline
import TypeCheck.StateMachine
import Utils

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
sink ToList _c q = flushQueue q

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
          Upgrade msock name' f' mg
            | name /= name' -> do
                writeQueue q' (Upgrade msock name' f' mg)
                go s f
            | otherwise -> case (cast f', cast s) of
                             (Just (f'' :: T s a b), Just s') -> do
                               writeQueue q' (UpgradeSucceeded Nothing name)
                               case mg of
                                 Nothing -> go s f''
                                 Just g  -> go (g s') undefined
                             _ -> do
                               writeQueue q' (UpgradeFailed Nothing name)
                               go s f
          Upgrade_ msock name' sty sty' a' b' f' mg -> do
            case tryUpgrade s f (UpgradeD_ sty sty' a' b' f' mg) of
              Just (SameState f') -> do
                writeQueue q' (UpgradeSucceeded msock name)
                go s f'
              Just (DifferentState g f') -> do
                writeQueue q' (UpgradeSucceeded msock name)
                let (_, s') = runT g s ()
                go s' f'
              Nothing -> do
                writeQueue q' (UpgradeFailed msock name)
                go s f
          UpgradeSucceeded msock name' -> do
            writeQueue q' (UpgradeSucceeded msock name')
            go s f
          UpgradeFailed msock name' -> do
            writeQueue q' (UpgradeFailed msock name')
            go s f
          Done -> do
            writeQueue q' Done
            return ()
  _pid <- forkIO (go s0 f0)
  return q'

------------------------------------------------------------------------

data UpgradeD_ = UpgradeD_ Ty_ Ty_ Ty_ Ty_ U U

data UpgradeD s s' a b where
  SameState :: T s a b -> UpgradeD s s' a b
  DifferentState :: T () s s' -> T s' a b -> UpgradeD s s' a b

tryUpgrade :: forall s s' a b. (Typeable s, Typeable s', Typeable a, Typeable b)
           => s -> T s a b -> UpgradeD_ -> Maybe (UpgradeD s s' a b)
tryUpgrade _s f (UpgradeD_ t_ t'_ a'_ b'_ f' g) = do
  case (inferTy t_, inferTy t'_, inferTy a'_, inferTy b'_) of
    (ETy (t :: Ty t), ETy (t' :: Ty t'), ETy (a' :: Ty a'), ETy (b' :: Ty b')) ->
      case (eqT @a @a', eqT @b @b', eqT @s @t) of
        (Just Refl, Just Refl, Just Refl) ->
          case typeCheck f' t a' b' of
            Right ff -> Just (SameState ff)
            Left err -> error (show err)
        _ -> error "tryUpdate"

              -- case (eqT @s @t, eqT @s' @t') of
              -- (Just Refl, Just Refl) -> case eqT @s @s' of
--                Just Refl -> Just (SameState ff)
--                Nothing -> case typeCheck g TUnit t t' of
--                  Right (gg :: T () s s') -> Just (DifferentState gg ff)
--                  Left err -> error (show err)
--              _ -> error "s /= s'"
--            Left err -> error (show err)

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
