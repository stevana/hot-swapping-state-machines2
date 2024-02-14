{-# LANGUAGE ScopedTypeVariables #-}

module Syntax.StateMachine.Untyped where

import Syntax.Types

------------------------------------------------------------------------

infixr 2 `CaseU`
infixr 3 :.&&

data U
  = IdU
  | ComposeU U U
  | FstU
  | SndU
  | IntU Int
  | CopyU
  | FirstU U
  | SecondU U
  | CaseU U U
  | (:.&&) U U
  | IncrU
  | ConsumeU
  | GetU
  | PutU
  | ReadU Ty_
  | ShowU Ty_
  deriving (Show, Read)

------------------------------------------------------------------------

infixr 1 .>>
(.>>) :: U -> U -> U
f .>> g = g `ComposeU` f
