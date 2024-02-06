{-# LANGUAGE GADTs #-}

module Syntax.StateMachine.Typed where

------------------------------------------------------------------------

data T a b where
  Id      :: T a a
  Compose :: T b c -> T a b -> T a c
  Fst     :: T (a, b) a
  Snd     :: T (a, b) b
  Copy    :: T a (a, a)
  Consume :: T a ()
  Int     :: Int -> T a Int
  Incr    :: T Int Int
  Loop    :: T (a, s) (b, s) -> T a b
  First   :: T a b -> T (a, c) (b, c)
  Second  :: T b c -> T (a, b) (a, c)
  (:&&&)  :: T a b -> T a c -> T a (b, c)
  Delay   :: a -> T a a
  (:|||)  :: T a c -> T b c -> T (Either a b) c
  (:+++)  :: T a c -> T b d -> T (Either a b) (Either c d)
  Distr   :: T (Either a b, c) (Either (a, c) (b, c))
  Distr'  :: T (Either (a, c) (b, c)) (Either a b, c)

------------------------------------------------------------------------

infixr 1 >>>
(>>>) :: T a b -> T b c -> T a c
f >>> g = g `Compose` f

infixr 3 ***
(***) :: T a c -> T b d -> T (a, b) (c, d)
f *** g = First f >>> Second g
