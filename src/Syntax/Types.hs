{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Syntax.Types where

import Data.Type.Equality
import Data.Typeable
import GHC.Types (Constraint)
import Unsafe.Coerce

------------------------------------------------------------------------

data Ty_
  = UTUnit
  | UTInt
  | UTBool
  | UTPair Ty_ Ty_
  | UTEither Ty_ Ty_
  deriving (Show, Read)

------------------------------------------------------------------------

data Ty a where
  TUnit   :: Ty ()
  TInt    :: Ty Int
  TBool   :: Ty Bool
  TString :: Ty String
  TPair   :: Ty a -> Ty b -> Ty (a, b)
  TEither :: Ty a -> Ty b -> Ty (Either a b)
  TDon'tCare :: Ty a
deriving instance Show (Ty a)

instance TestEquality Ty where
  testEquality TUnit TUnit = Just Refl
  testEquality TInt  TInt = Just Refl
  testEquality TBool TBool = Just Refl
  testEquality TString TString = Just Refl
  testEquality (TPair l r) (TPair l' r') = case (testEquality l l', testEquality r r') of
    (Just Refl, Just Refl) -> Just Refl
    _ -> Nothing
  testEquality (TEither l r) (TEither l' r') = case (testEquality l l', testEquality r r') of
    (Just Refl, Just Refl) -> Just Refl
    _ -> Nothing
  testEquality TDon'tCare _b = unsafeCoerce (Just Refl)
  testEquality _a TDon'tCare = unsafeCoerce (Just Refl)
  testEquality _ _ = Nothing

------------------------------------------------------------------------

data ETy where
  ETy :: Typeable a => Ty a -> ETy

inferTy :: Ty_ -> ETy
inferTy UTUnit = ETy TUnit
inferTy UTInt  = ETy TInt
inferTy UTBool = ETy TBool
inferTy (UTPair ua ub) = case (inferTy ua, inferTy ub) of
  (ETy a, ETy b) -> ETy (TPair a b)
inferTy (UTEither ua ub) = case (inferTy ua, inferTy ub) of
  (ETy a, ETy b) -> ETy (TEither a b)

------------------------------------------------------------------------

data Proof (c :: Constraint) where
  Witness :: c => Proof c

inferShow :: Ty a -> Maybe (Proof (Show a))
inferShow TUnit = return Witness
inferShow TBool = return Witness
inferShow TInt = return Witness
inferShow (TPair a b) = do
  Witness <- inferShow a
  Witness <- inferShow b
  return Witness
inferShow (TEither a b) = do
  Witness <- inferShow a
  Witness <- inferShow b
  return Witness
inferShow TDon'tCare = error "inferShow: don't care"

inferRead :: Ty a -> Maybe (Proof (Read a))
inferRead TUnit = return Witness
inferRead TBool = return Witness
inferRead TInt = return Witness
inferRead (TPair a b) = do
  Witness <- inferRead a
  Witness <- inferRead b
  return Witness
inferRead (TEither a b) = do
  Witness <- inferRead a
  Witness <- inferRead b
  return Witness
inferRead TDon'tCare = error "inferRead: don't care"

inferTypeable :: Ty a -> Maybe (Proof (Typeable a))
inferTypeable TUnit = return Witness
inferTypeable TBool = return Witness
inferTypeable TInt = return Witness
inferTypeable (TPair a b) = do
  Witness <- inferTypeable a
  Witness <- inferTypeable b
  return Witness
inferTypeable (TEither a b) = do
  Witness <- inferTypeable a
  Witness <- inferTypeable b
  return Witness
inferTypeable TDon'tCare = error "inferTypeable: don't care"
