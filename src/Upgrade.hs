{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Upgrade where

import Data.Typeable

import Syntax.StateMachine.Typed
import Syntax.StateMachine.Untyped
import Syntax.Types
import TypeCheck.StateMachine

------------------------------------------------------------------------

data UpgradeData_ = UpgradeData_
  { oldState        :: Ty_
  , newState        :: Ty_
  , newInput        :: Ty_
  , newOutput       :: Ty_
  , newStateMachine :: U
  , stateMigration  :: U
  }
  deriving (Show, Read)

data UpgradeData s a b = UpgradeData (T s a b) (T () s s)
  deriving Show

(<?>) :: Either a b -> a' -> Either a' b
Left _x <?> x' = Left x'
Right y <?> _  = Right y

typeCheckUpgrade :: forall s s' a b. (Typeable s, Typeable a, Typeable b)
                 => s -> T s a b -> UpgradeData_ -> Either TypeError (UpgradeData s a b)
typeCheckUpgrade _s _f (UpgradeData_ t_ t'_ a'_ b'_ f_ g_) =
  case (inferTy t_, inferTy t'_, inferTy a'_, inferTy b'_) of
    (ETy (t :: Ty t), ETy (t' :: Ty t'), ETy (a' :: Ty a'), ETy (b' :: Ty b')) -> do
      Refl <- decT @a @a' <?> InputMismatch a'_
      Refl <- decT @b @b' <?> OutputMismatch b'_
      Refl <- decT @s @t  <?> OldStateMismatch t_
      f <- typeCheck f_ t a' b'
      g <- typeCheck g_ TUnit t t
      return (UpgradeData f g)
