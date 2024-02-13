{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Interpreter where

import Control.Monad
import Control.Arrow (first, loop, second)
import qualified Control.Arrow as A
import Data.Bifunctor (bimap)
import Control.Monad.State

import Stream
import Syntax.StateMachine.Typed

------------------------------------------------------------------------

eval :: T s a b -> a -> State s b
eval Id            = return
eval (Compose g f) = eval g <=< eval f
eval Fst           = return . fst
eval Snd           = return . snd
eval Copy          = return . (\x -> (x, x))
eval Consume       = return . const ()
eval (Int i)       = return . const i
eval Incr          = return . succ
eval Get           = const get
eval Put           = put
eval (First f)     = \(x, y) -> eval f x >>= \x' -> return (x', y)
eval (Second g)    = \(x, y) -> eval g y >>= \y' -> return (x, y')
eval (f :&&& g)    = \x -> (,) <$> eval f x <*> eval g x
eval (_f :||| _g)  = undefined
eval (Case f g)    = either (fmap Left . eval f) (fmap Right . eval g)
eval Read          = return . read
eval Show          = return . show
eval _ = undefined

runT :: T s a b -> a -> s -> (b, s)
runT f x s = runState (eval f x) s

------------------------------------------------------------------------

evalS :: T s a b -> Stream a -> Stream b
evalS Id            = id
evalS (Compose g f) = evalS g . evalS f
evalS Fst           = mapS fst
evalS Snd           = mapS snd
evalS Copy          = mapS (\x -> (x, x))
evalS Consume       = mapS (const ())
evalS (Int i)       = mapS (const i)
evalS Incr          = mapS (+1)
-- evalS (Loop f)      = loop (unzipS . evalS f . zipS)
evalS (First f)     = zipS . first (evalS f) . unzipS
evalS (Second g)    = zipS . second (evalS g) . unzipS
evalS (f :&&& g)    = zipS . (evalS f A.*** evalS g) . unzipS . evalS Copy -- XXX: awkward
-- evalS (Delay x)     = undefined -- Cons x
evalS (_f :||| _g)    = undefined -- mapS $ either (evalS f) (evalS g)
evalS (Case f g)    = \xs -> combineS xs . bimap (evalS f) (evalS g) . splitS $ xs -- mapS . bimap (evalS f) (evalS g)
-- evalS Distr         = mapS $ \(e, c) -> case e of
--                                   Left  x -> Left  (x, c)
--                                   Right y -> Right (y, c)
-- evalS Distr'        = mapS $ \e -> case e of
--                              Left  (x, c) -> (Left x, c)
--                              Right (y, c) -> (Right y, c)

runS :: T s a b -> [a] -> [b]
runS f xs = take (length xs) (toListS (evalS f (fromListS xs)))
