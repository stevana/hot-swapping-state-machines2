{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Syntax.StateMachine.Typed where

import Control.Category

------------------------------------------------------------------------

infixr 2 `Case`
-- infixr 2 :|||
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
  -- (:|||)  :: T s a c -> T s b c -> T s (Either a b) c
  Case    :: T s a c -> T s b d -> T s (Either a b) (Either c d)

  -- Read and update the state.
  Get     :: T s () s
  Put     :: T s s ()

  -- Converting values from and to strings.
  Read    :: Read a => T s String a
  Show    :: Show a => T s a String

  Not :: T s Bool Bool
  If  :: T s a b -> T s a b -> T s (a, Bool) b
  Decr :: T s Int Int
  Bool :: Bool -> T s a Bool -- XXX: why doesn't this work if the input type is ()?

  Unleft :: T s (Either a c) (Either b c) -> T s a b

  Eq  :: Eq a => T s (a, a) (Either () ())

  Loop    :: T s (a, s) (b, s) -> T s a b
  Delay   :: Show a => a -> T s a a
  -- Distr   :: T s (Either a b, c) (Either (a, c) (b, c))
  -- Distr'  :: T s (Either (a, c) (b, c)) (Either a b, c)

deriving instance Show (T s a b)

------------------------------------------------------------------------

instance Category (T s) where
  id = Id
  (.) = Compose

infixr 3 ***
(***) :: T s a c -> T s b d -> T s (a, b) (c, d)
f *** g = First f >>> Second g
