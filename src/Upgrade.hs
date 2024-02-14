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
  { oldState       :: Ty_
  , newState       :: Ty_
  , newInput       :: Ty_
  , newOutput      :: Ty_
  , newSM          :: U
  , stateMigration :: U
  }
  deriving (Show, Read)

data UpgradeD s s' a b where
  SameState :: T s a b -> UpgradeD s s' a b
  DifferentState :: T () s s' -> T s' a b -> UpgradeD s s' a b

tryUpgrade :: forall s s' a b. (Typeable s, Typeable s', Typeable a, Typeable b)
           => s -> T s a b -> UpgradeData_ -> Maybe (UpgradeD s s' a b)
tryUpgrade _s f (UpgradeData_ t_ t'_ a'_ b'_ f' g) = do
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
