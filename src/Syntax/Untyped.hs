module Syntax.Untyped where

import GHC.Types (Any)

------------------------------------------------------------------------

newtype Opaque a = Opaque a

instance Show (Opaque a) where
  show _ = "<opaque>"

------------------------------------------------------------------------

data Ty_
  = UTUnit
  | UTInt
  | UTBool
  | UTPair Ty_ Ty_
  | UTEither Ty_ Ty_
  deriving Show

------------------------------------------------------------------------

data U
  = IdU
  | ComposeU U U
  | FstU
  | SndU
  | IntU Int
  | LoopU Ty_ U
  | DistrU
  | DistrU'
  | CopyU
  | SecondU U
  | UnsecondU U
  | DelayU (Opaque Any)
  | (:.++) U U
  | (:.&&) U U
  | IncrU
  | ConsumeU
  deriving Show

------------------------------------------------------------------------

infixr 1 .>>
(.>>) :: U -> U -> U
f .>> g = g `ComposeU` f
