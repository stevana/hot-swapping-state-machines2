{-# LANGUAGE ScopedTypeVariables #-}

module Syntax.StateMachine.Untyped where

import Control.Applicative
import GHC.Read
import Text.Read

import Syntax.Types

------------------------------------------------------------------------

infixr 2 :.++
infixr 3 :.&&

data U
  = IdU
  | ComposeU U U
  | FstU
  | SndU
  | IntU Int
  | CopyU
  | SecondU U
  | (:.++) U U
  | (:.&&) U U
  | IncrU
  | ConsumeU
  | GetU
  | PutU
  | ReadU Ty_
  | ShowU Ty_
  deriving Show

instance Read U where
  readPrec = choose
    [ ("IdU",      return IdU)
    , ("ComposeU", ComposeU <$> parens readPrec <*> parens readPrec)
    , ("FstU",     return FstU)
    , ("SndU",     return SndU)
    , ("IntU",     IntU <$> parens readPrec)
    , ("CopyU",    return CopyU)
    , ("SecondU",  SecondU <$> parens readPrec)
    , ("IncrU",    return IncrU)
    , ("ConsumeU", return ConsumeU)
    , ("GetU",     return GetU)
    , ("PutU",     return PutU)
    , ("ReadU",    ReadU <$> readPrec)
    , ("ShowU",    ShowU <$> readPrec)
    ] <|> choiceP <|> fanoutP
    where
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
