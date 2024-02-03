{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Syntax.Typed where

import Unsafe.Coerce
import Data.Type.Equality

------------------------------------------------------------------------

data Ty a where
  TUnit   :: Ty ()
  TInt    :: Ty Int
  TBool   :: Ty Bool
  TPair   :: Ty a -> Ty b -> Ty (a, b)
  TEither :: Ty a -> Ty b -> Ty (Either a b)
  TDon'tCare :: Ty a
deriving instance Show (Ty a)

instance TestEquality Ty where
  testEquality TUnit TUnit = Just Refl
  testEquality TInt  TInt = Just Refl
  testEquality TBool TBool = Just Refl
  testEquality (TPair l r) (TPair l' r') = case (testEquality l l', testEquality r r') of
    (Just Refl, Just Refl) -> Just Refl
    _ -> Nothing
  testEquality (TEither l r) (TEither l' r') = case (testEquality l l', testEquality r r') of
    (Just Refl, Just Refl) -> Just Refl
    _ -> Nothing
  testEquality TDon'tCare b = unsafeCoerce (Just Refl)
  testEquality a TDon'tCare = unsafeCoerce (Just Refl)
  testEquality _ _ = Nothing

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
