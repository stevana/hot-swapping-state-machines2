{-# LANGUAGE GADTs #-}

module TypeCheck.StateMachine where

import Data.Type.Equality

import Syntax.StateMachine.Typed
import Syntax.StateMachine.Untyped
import Syntax.Types

------------------------------------------------------------------------

data TypeError = IdTE | ComposeTE | CopyTE | FstTE | SndTE | SecondTE | LoopTE | Distr'TE
  | GetTE | PutTE
  deriving Show

throw :: TypeError -> Either TypeError a
throw = Left

------------------------------------------------------------------------

typeCheck :: U -> Ty s -> Ty a -> Ty b -> Either TypeError (T s a b)
typeCheck IdU _s a b = case testEquality a b of
  Just Refl -> return Id
  Nothing   -> throw IdTE
typeCheck (ComposeU ug uf) s a c = do
  EI b  g <- inferI ug s c
  EO b' f <- inferO uf s a
  case testEquality b b' of
    Just Refl -> return (Compose g f)
    _ -> throw ComposeTE
typeCheck FstU _s (TPair a _b) a' =
  case testEquality a a' of
    Just Refl -> return Fst
    Nothing -> throw FstTE
typeCheck FstU _ _ _ = throw FstTE
typeCheck SndU _ (TPair _a b) b' =
  case testEquality b b' of
    Just Refl -> return Snd
    Nothing -> throw SndTE
typeCheck CopyU _ a (TPair a' a'') = do
  case (testEquality a a', testEquality a a'') of
    (Just Refl, Just Refl) -> return Copy
    _ -> throw CopyTE
typeCheck GetU s TUnit s' = case testEquality s s' of
  Just Refl -> return Get
  Nothing -> throw GetTE
typeCheck PutU s s' TUnit = case testEquality s s' of
  Just Refl -> return Put
  Nothing -> throw PutTE
typeCheck ConsumeU _s _a TUnit = return Consume
typeCheck IncrU _s TInt TInt = return Incr
typeCheck (SecondU ug) s (TPair a b) (TPair a' c) = do
  case testEquality a a' of
    Just Refl -> do
      EI b' g <- inferI ug s c
      case testEquality b b' of
        Just Refl -> return (Second g)
        _ -> error ""
    _ -> error ""
typeCheck (uf :.&& ug) s a (TPair b c) = do
  f <- typeCheck uf s a b
  g <- typeCheck ug s a c
  return (f :&&& g)
typeCheck (uf :.++ ug) s (TEither a b) (TEither c d) = do
  f <- typeCheck uf s a c
  g <- typeCheck ug s b d
  return (Case f g)
typeCheck (ReadU _ua) _s TString a =
  case inferRead a of
    Just Witness -> return Read
typeCheck (ShowU _ua) _s a TString =
  case inferShow a of
    Just Witness -> return Show
typeCheck u s a b = error (show (u, s, a, b))

------------------------------------------------------------------------

-- We can't infer both input and output types of terms:

-- data E where
--   E :: Ty a -> Ty b -> T a b -> E
--
-- infer :: U -> Either E
-- infer IdU = return (E _ _ Id) -- doesn't work, multiple possibilities!

-- But we can infer either the output given the input or vice versa:

data EO s a where
  EO :: Ty b -> T s a b -> EO s a

data EI s b where
  EI :: Ty a -> T s a b -> EI s b

inferO :: U -> Ty s -> Ty a -> Either TypeError (EO s a)
inferO IdU _s a  = return (EO a Id)
inferO (ComposeU ug uf) s a = do
  EO b f <- inferO uf s a
  EO c g <- inferO ug s b
  return (EO c (Compose g f))
inferO FstU _s (TPair a _b) = return (EO a Fst)
inferO SndU _s (TPair _a b) = return (EO b Snd)
inferO (SecondU ug) s (TPair a b) = do
  EO c g <- inferO ug s b
  return (EO (TPair a c) (Second g))
inferO (ReadU ua) _s TString = do
  case inferTy ua of
    ETy a -> case inferRead a of
      Just Witness -> return (EO a Read)
inferO u s t = error ("inferO: " ++ show (u, s, t))

inferI :: U -> Ty s -> Ty b -> Either TypeError (EI s b)
inferI IdU _s a  = return (EI a Id)
inferI (ComposeU ug uf) s c = do
  EI b g <- inferI ug s c
  EI a f <- inferI uf s b
  return (EI a (Compose g f))
inferI FstU _s a = return (EI (TPair a TDon'tCare) Fst)
inferI (SecondU ug) s (TPair a c) = do
  EI b g <- inferI ug s c
  return (EI (TPair a b) (Second g))
inferI (SecondU _ug) _s _ = throw SecondTE
inferI (uf :.++ ug) s (TEither c d) = do
  EI a f <- inferI uf s c
  EI b g <- inferI ug s d
  return (EI (TEither a b) (Case f g))
inferI (uf :.&& ug) s (TPair b c) = do
  EI a  f <- inferI uf s b
  EI a' g <- inferI ug s c
  case testEquality a a' of
    Just Refl -> return (EI a (f :&&& g))
    Nothing -> error $ "&&&: " ++ show (a, a')
inferI CopyU _s (TPair a a') =
  case testEquality a a' of
    Just Refl -> return (EI a Copy)
    Nothing -> error "copy"
inferI SndU _s a = return (EI (TPair TDon'tCare a) Snd)
inferI ConsumeU _s TUnit = return (EI TDon'tCare Consume)
inferI IncrU _s TInt = return (EI TInt Incr)
inferI (ShowU ua) _s TString = do
  case inferTy ua of
    ETy a -> case inferShow a of
      Just Witness -> return (EI a Show)
inferI GetU s s' = case testEquality s s' of
   Just Refl -> return (EI TUnit Get)
   Nothing -> error $ "inferI: GetU: s=" ++ show s ++ ", s'= " ++ show s'
inferI PutU s TUnit = return (EI s Put)
inferI (IntU i) _s TInt = return (EI TUnit (Int i))
inferI u s a  = error ("inferI:" ++ show (u, s, a))
