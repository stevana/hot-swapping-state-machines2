{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module TypeCheck.Pipeline where

import Data.Typeable
import Data.Dynamic
import Data.Type.Equality

import Syntax.Pipeline.Untyped
import Syntax.Pipeline.Typed
import Syntax.Types
import TypeCheck.StateMachine

------------------------------------------------------------------------

data EP a b where
  EP :: Proof (Read a) -> Proof (Show b) -> P a b -> EP a b

typeCheckP :: (Typeable a, Typeable b) => Ty a -> Ty b -> UP -> Maybe (EP a b)
typeCheckP a b IdPU = do
  Refl <- testEquality a b
  p <- inferRead a
  q <- inferShow b
  return (EP p q IdP)
typeCheckP a c (uf :.>> ug) = do
  EPO b  f <- inferOP a uf
  EPI b' g <- inferIP c ug
  case testEquality b b' of
    Just Refl -> case (inferRead a, inferShow c, inferTypeable b) of
      (Just p, Just q, Just Witness) -> return (EP p q (f :>>> g))
      _ -> error "typeCheckP: failed to infer needed instances"
    Nothing -> error "typeCheckP: functions don't compose"
typeCheckP a b (SMU name sty_ us uf) =
  case inferTy sty_ of
    ETy (sty :: Ty s) -> do
      Witness <- inferTypeable sty
      s <- fromDynamic us :: Maybe s
      case typeCheck uf sty a b of
        Left err -> error (show err)
        Right f -> do
          p <- inferRead a
          q <- inferShow b
          return (EP p q (SM name s f))

data EPO a where
  EPO :: Ty b -> P a b -> EPO a

data EPI b where
  EPI :: Ty a -> P a b -> EPI b

inferOP :: Ty a -> UP -> Maybe (EPO a)
inferOP a IdPU = return (EPO a IdP)
inferOP a (uf :.>> ug) = do
  EPO b f <- inferOP a uf
  EPO c g <- inferOP b ug
  Witness <- inferTypeable b
  return (EPO c (f :>>> g))
inferOP a (SMU name sty_ us uf) =
  case inferTy sty_ of
    ETy (sty :: Ty s) -> do
      case inferO uf sty a of
        Right (EO b f) -> case fromDynamic us of
          Just (s :: s) -> return (EPO b (SM name s f))
          Nothing -> error "inferOP: SMU"
        Left err -> error (show err)

inferIP :: Ty b -> UP -> Maybe (EPI b)
inferIP b IdPU = return (EPI b IdP)
inferIP c (uf :.>> ug) = do
  EPI b g <- inferIP c ug
  EPI a f <- inferIP b uf
  Witness <- inferTypeable b
  return (EPI a (f :>>> g))
inferIP b (SMU name sty_ us uf) =
  case inferTy sty_ of
    ETy (sty :: Ty s) -> do
      case inferI uf sty b of
        Right (EI a f) -> case fromDynamic us of
          Just (s :: s) -> return (EPI a (SM name s f))
          Nothing -> error "inferIP: SMU"
        Left err -> error (show err)
