{-# LANGUAGE GADTs #-}

module Erase where

import Syntax.StateMachine.Typed
import Syntax.StateMachine.Untyped

------------------------------------------------------------------------

erase :: T s a b -> U
erase Id            = IdU
erase (Compose g f) = ComposeU (erase g) (erase f)
erase (Int i)       = IntU i
erase Incr          = IncrU
erase Consume       = ConsumeU
erase Fst           = FstU
erase Snd           = SndU
erase Copy          = CopyU
erase (First f)     = FirstU (erase f)
erase (Second g)    = SecondU (erase g)
erase (f :&&& g)    = erase f :.&& erase g
erase (Case f g)    = CaseU (erase f) (erase g)
erase Get           = GetU
erase Put           = PutU
erase Read          = ReadU (error "XXX: awkward")
erase Show          = ShowU (error "XXX: awkward")
erase _             = undefined
