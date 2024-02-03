{-# LANGUAGE GADTs #-}

module Interpreter where

import Control.Arrow (first, loop, second)
import qualified Control.Arrow as A
import Data.Bifunctor (bimap)

import Stream
import Syntax.Typed

------------------------------------------------------------------------

eval :: T a b -> Stream a -> Stream b
eval Id            = id
eval (Compose g f) = eval g . eval f
eval Fst           = mapS fst
eval Snd           = mapS snd
eval Copy          = mapS (\x -> (x, x))
eval Consume       = mapS (const ())
eval (Int i)       = mapS (const i)
eval Incr          = mapS (+1)
eval (Loop f)      = loop (unzipS . eval f . zipS)
eval (First f)     = zipS . first (eval f) . unzipS
eval (Second g)    = zipS . second (eval g) . unzipS
eval (f :&&& g)    = zipS . (eval f A.*** eval g) . unzipS . eval Copy -- XXX: awkward
eval (Delay x)     = Cons x
eval (_f :||| _g)    = undefined -- mapS $ either (eval f) (eval g)
eval (f :+++ g)    = \xs -> combineS xs . bimap (eval f) (eval g) . splitS $ xs -- mapS . bimap (eval f) (eval g)
eval Distr         = mapS $ \(e, c) -> case e of
                                  Left  x -> Left  (x, c)
                                  Right y -> Right (y, c)
eval Distr'        = mapS $ \e -> case e of
                             Left  (x, c) -> (Left x, c)
                             Right (y, c) -> (Right y, c)

run :: T a b -> [a] -> [b]
run f xs = take (length xs) (toListS (eval f (fromListS xs)))
