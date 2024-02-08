{-# LANGUAGE ScopedTypeVariables #-}

module Syntax.Pipeline.Untyped where

import Text.Read
import GHC.Read
import Data.Dynamic

import Syntax.Types
import Syntax.StateMachine.Untyped

------------------------------------------------------------------------

data UP = IdPU | (:.>>) UP UP | SMU String Ty_ Dynamic U
  deriving Show

instance Read UP where
  readPrec = choose
    [ ("IdPU", return IdPU)
    , (":.>>", (:.>>) <$> readPrec <*> readPrec)
    , ("SMU", smuP)
    ]
    where
      smuP = do
        name <- readPrec
        sty_ <- readPrec
        case inferTy sty_ of
          ETy (sty :: Ty s) -> do
            case inferRead sty of
              Just Witness -> do
                (s :: s) <- readPrec
                f <- parens readPrec
                return (SMU name sty_ (toDyn s) f)
