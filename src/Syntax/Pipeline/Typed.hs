{-# LANGUAGE GADTs #-}

module Syntax.Pipeline.Typed where

import Data.Typeable

import Syntax.StateMachine.Typed

------------------------------------------------------------------------

data P a b where
  IdP    :: P a a
  (:>>>) :: Typeable b => P a b -> P b c -> P a c
  SM     :: Typeable s => Name -> s -> T s a b -> P a b
  -- Module    :: Name -> P a b -> P a b
  -- ^ So we can upgrade bigger parts of the pipeline?!

  -- Tee :: P a ByteString -> Sink -> P a a?

type Name = String
