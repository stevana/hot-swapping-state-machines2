{-# LANGUAGE ScopedTypeVariables #-}

module Syntax.StateMachine.Untyped where

import Control.Applicative
import GHC.Read
import GHC.Types (Any)
import Text.Read
import Unsafe.Coerce

import Syntax.Types

------------------------------------------------------------------------

newtype Opaque a = Opaque a

instance Show (Opaque a) where
  show _ = "<opaque>"

------------------------------------------------------------------------

infixr 2 :.++
infixr 3 :.&&

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
  | DelayU Ty_ (Opaque Any)
  | (:.++) U U
  | (:.&&) U U
  | IncrU
  | ConsumeU
  deriving Show

instance Read U where
  readPrec = choose
    [ ("IdU",      return IdU)
    , ("ComposeU", ComposeU <$> parens readPrec <*> parens readPrec)
    , ("FstU",     return FstU)
    , ("SndU",     return SndU)
    , ("IntU",     IntU <$> parens readPrec)
    , ("LoopU",    LoopU <$> parens readPrec <*> parens readPrec)
    , ("DistrU",   return DistrU)
    , ("DistrU'",  return DistrU')
    , ("CopyU",    return CopyU)
    , ("SecondU",  SecondU <$> parens readPrec)
    , ("DelayU",   delayP)
    , ("IncrU",    return IncrU)
    , ("ConsumeU", return ConsumeU)
    ] <|> choiceP <|> fanoutP
    where
      delayP = do
        ua <- parens readPrec
        case inferTy ua of
          ETy (t :: Ty a) ->
            case inferRead t of
              Just Witness -> do
                (i :: a) <- parens readPrec
                return (DelayU ua (Opaque (unsafeCoerce i)))
              Nothing -> error "read: DelayU"

      choiceP = prefixP
        where
          prefixP = prec 5 $ do
            Symbol ":.++" <- parens lexP
            (:.++) <$> step (parens readPrec) <*> step (parens readPrec)

          -- infixP = prec 5 $ do
          --   f <- step (parens readPrec)
          --   Symbol ":.++" <- lexP
          --   g <- step (parens readPrec)
          --   return (f :.++ g)

      fanoutP = prefixP
        where
          prefixP = prec 5 $ do
            Symbol ":.&&" <- parens lexP
            (:.&&) <$> step (parens readPrec) <*> step (parens readPrec)

------------------------------------------------------------------------

infixr 1 .>>
(.>>) :: U -> U -> U
f .>> g = g `ComposeU` f
