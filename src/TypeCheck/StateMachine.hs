{-# LANGUAGE GADTs #-}

module TypeCheck.StateMachine where

import Data.Type.Equality
import Unsafe.Coerce

import Syntax.StateMachine.Typed
import Syntax.StateMachine.Untyped
import Syntax.Types

------------------------------------------------------------------------

data TypeError = IdTE | ComposeTE | CopyTE | FstTE | SndTE | SecondTE | LoopTE | Distr'TE
  deriving Show

throw :: TypeError -> Either TypeError a
throw = Left

------------------------------------------------------------------------

typeCheck :: U -> Ty a -> Ty b -> Either TypeError (T a b)
typeCheck IdU TInt  TInt  = return Id
typeCheck IdU TBool TBool = return Id
typeCheck IdU (TEither a b) (TEither a' b') =
  case (testEquality a a', testEquality b b') of
    (Just Refl, Just Refl) -> return Id
    _ -> error "typeCheck IdU"
typeCheck IdU (TPair a b) (TPair a' b') =
  case (testEquality a a', testEquality b b') of
    (Just Refl, Just Refl) -> return Id
    _ -> error "typeCheck: IdU Pair"
typeCheck IdU _ _ = throw IdTE
typeCheck (ComposeU ug uf) a c = do
  EI b  g <- inferI ug c
  EO b' f <- inferO uf a
  case testEquality b b' of
    Just Refl -> return (Compose g f)
    _ -> throw ComposeTE
typeCheck FstU (TPair a _b) a' =
  case testEquality a a' of
    Just Refl -> return Fst
    Nothing -> throw FstTE
typeCheck FstU _ _ = throw FstTE
typeCheck SndU (TPair _a b) b' =
  case testEquality b b' of
    Just Refl -> return Snd
    Nothing -> throw SndTE
typeCheck CopyU a (TPair a' a'') = do
  case (testEquality a a', testEquality a a'') of
    (Just Refl, Just Refl) -> return Copy
    _ -> throw CopyTE
typeCheck ConsumeU _a TUnit = return Consume
typeCheck IncrU TInt TInt = return Incr
typeCheck (SecondU ug) (TPair a b) (TPair a' c) = do
  case testEquality a a' of
    Just Refl -> do
      EI b' g <- inferI ug c
      case testEquality b b' of
        Just Refl -> return (Second g)
        _ -> error ""
    _ -> error ""
typeCheck (LoopU us uf) a b = do
  case inferTy us of
    ETy s ->
      case inferI uf (TPair b s) of
        Right (EI (TPair a' s') _) -> do
          case (testEquality a a', testEquality s s') of
            (Just Refl, Just Refl) -> do
              f <- typeCheck uf (TPair a s) (TPair b s)
              return (Loop f)
            _ -> throw LoopTE
        Right _ -> throw LoopTE
        Left err -> throw err
typeCheck (DelayU _ty (Opaque x)) a a' = do
  case testEquality a a' of
    Just Refl -> return (Delay (unsafeCoerce x))
    _ -> error ""
typeCheck (uf :.&& ug) a (TPair b c) = do
  f <- typeCheck uf a b
  g <- typeCheck ug a c
  return (f :&&& g)
typeCheck (uf :.++ ug) (TEither a b) (TEither c d) = do
  f <- typeCheck uf a c
  g <- typeCheck ug b d
  return (f :+++ g)
typeCheck DistrU (TPair (TEither a b) c) (TEither (TPair a' c') (TPair b' c'')) =
  case (testEquality a a', testEquality b b', testEquality c c', testEquality c' c'') of
    (Just Refl, Just Refl, Just Refl, Just Refl) -> return Distr
    _ -> error ""
typeCheck DistrU' (TEither (TPair a c) (TPair b c')) (TPair (TEither a' b') c'') =
  case (testEquality a a', testEquality b b', testEquality c c', testEquality c' c'') of
    (Just Refl, Just Refl, Just Refl, Just Refl) -> return Distr'
    _ -> error "typeCheck: DistrU'"
typeCheck u a b = error (show (u, a, b))

------------------------------------------------------------------------

-- We can't infer both input and output types of terms:

-- data E where
--   E :: Ty a -> Ty b -> T a b -> E
--
-- infer :: U -> Either E
-- infer IdU = return (E _ _ Id) -- doesn't work, multiple possibilities!

-- But we can infer either the output given the input or vice versa:

data EO a where
  EO :: Ty b -> T a b -> EO a

data EI b where
  EI :: Ty a -> T a b -> EI b

inferO :: U -> Ty a -> Either TypeError (EO a)
inferO IdU TInt  = return (EO TInt Id)
inferO IdU TBool = return (EO TBool Id)
inferO (ComposeU ug uf) a = do
  EO b f <- inferO uf a
  EO c g <- inferO ug b
  return (EO c (Compose g f))
inferO FstU (TPair a _b) = return (EO a Fst)
inferO SndU (TPair _a b) = return (EO b Snd)
inferO (SecondU ug) (TPair a b) = do
  EO c g <- inferO ug b
  return (EO (TPair a c) (Second g))
inferO (DelayU _ty (Opaque x)) a = return (EO a (Delay (unsafeCoerce x)))
inferO u t = error ("inferO: " ++ show (u, t))

inferI :: U -> Ty b -> Either TypeError (EI b)
inferI IdU a  = return (EI a Id)
inferI (ComposeU ug uf) c = do
  EI b g <- inferI ug c
  EI a f <- inferI uf b
  return (EI a (Compose g f))
inferI FstU a = return (EI (TPair a TDon'tCare) Fst)
inferI (SecondU ug) (TPair a c) = do
  EI b g <- inferI ug c
  return (EI (TPair a b) (Second g))
inferI (SecondU _ug) _ = throw SecondTE
inferI DistrU' (TPair (TEither a b) c) = return (EI (TEither (TPair a c) (TPair b c)) Distr')
inferI (uf :.++ ug) (TEither c d) = do
  EI a f <- inferI uf c
  EI b g <- inferI ug d
  return (EI (TEither a b) (f :+++ g))
inferI (uf :.&& ug) (TPair b c) = do
  EI a  f <- inferI uf b
  EI a' g <- inferI ug c
  case testEquality a a' of
    Just Refl -> return (EI a (f :&&& g))
    Nothing -> error $ "&&&: " ++ show (a, a')
inferI CopyU (TPair a a') =
  case testEquality a a' of
    Just Refl -> return (EI a Copy)
    Nothing -> error "copy"
inferI SndU a = return (EI (TPair TDon'tCare a) Snd)
inferI ConsumeU TUnit = return (EI TDon'tCare Consume)
inferI IncrU TInt = return (EI TInt Incr)
inferI DistrU (TEither (TPair a c) (TPair b c')) =
  case testEquality c c' of
    Just Refl -> return (EI (TPair (TEither a b) c) Distr)
    Nothing -> error ("inferI: DistrU: " ++ show (c, c'))
inferI (DelayU _ty (Opaque x)) a = return (EI a (Delay (unsafeCoerce x)))
inferI u a  = error ("inferI:" ++ show (u, a))
