{-# LANGUAGE GADTs #-}

module Syntax.StateMachine.Typed where

------------------------------------------------------------------------

data T s a b where
  Id      :: T s a a
  Compose :: T s b c -> T s a b -> T s a c
  Fst     :: T s (a, b) a
  Snd     :: T s (a, b) b
  Copy    :: T s a (a, a)
  Consume :: T s a ()
  Int     :: Int -> T s a Int
  Incr    :: T s Int Int
  Get     :: T s () s
  Put     :: T s s ()
  -- Loop    :: T s (a, s) (b, s) -> T s a b
  First   :: T s a b -> T s (a, c) (b, c)
  Second  :: T s b c -> T s (a, b) (a, c)
  (:&&&)  :: T s a b -> T s a c -> T s a (b, c)
  -- Delay   :: a -> T s a a
  (:|||)  :: T s a c -> T s b c -> T s (Either a b) c
  (:+++)  :: T s a c -> T s b d -> T s (Either a b) (Either c d)
  Distr   :: T s (Either a b, c) (Either (a, c) (b, c))
  Distr'  :: T s (Either (a, c) (b, c)) (Either a b, c)
  Read    :: Read a => T s String a
  Show    :: Show a => T s a String


------------------------------------------------------------------------

infixr 1 >>>
(>>>) :: T s a b -> T s b c -> T s a c
f >>> g = g `Compose` f

infixr 3 ***
(***) :: T s a c -> T s b d -> T s (a, b) (c, d)
f *** g = First f >>> Second g
