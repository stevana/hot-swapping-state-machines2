{-# LANGUAGE GADTs #-}

module Syntax.StateMachine.Typed where

------------------------------------------------------------------------

infixr 2 `Case`
infixr 2 :|||
infixr 3 :&&&

data T s a b where
  -- Identity and composition.
  Id      :: T s a a
  Compose :: T s b c -> T s a b -> T s a c

  -- Introducing and incrementing integers.
  Int     :: Int -> T s () Int
  Incr    :: T s Int Int

  -- Working with product types.
  Consume :: T s a ()
  Fst     :: T s (a, b) a
  Snd     :: T s (a, b) b
  Copy    :: T s a (a, a)
  First   :: T s a b -> T s (a, c) (b, c)
  Second  :: T s b c -> T s (a, b) (a, c)
  (:&&&)  :: T s a b -> T s a c -> T s a (b, c)

  -- Working with sum types.
  (:|||)  :: T s a c -> T s b c -> T s (Either a b) c
  Case    :: T s a c -> T s b d -> T s (Either a b) (Either c d)

  -- Read and update the state.
  Get     :: T s () s
  Put     :: T s s ()

  -- Converting values from and to strings.
  Read    :: Read a => T s String a
  Show    :: Show a => T s a String

  -- Loop    :: T s (a, s) (b, s) -> T s a b
  -- Delay   :: a -> T s a a
  -- Distr   :: T s (Either a b, c) (Either (a, c) (b, c))
  -- Distr'  :: T s (Either (a, c) (b, c)) (Either a b, c)

------------------------------------------------------------------------

infixr 1 >>>
(>>>) :: T s a b -> T s b c -> T s a c
f >>> g = g `Compose` f

infixr 3 ***
(***) :: T s a c -> T s b d -> T s (a, b) (c, d)
f *** g = First f >>> Second g
