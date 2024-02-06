{-# LANGUAGE GADTs #-}

module Syntax.Pipeline.Typed where

import Data.Typeable

------------------------------------------------------------------------

type SM s i o = i -> s -> (s, o)

data P a b where
  IdP    :: P a a
  (:>>>) :: Typeable b => P a b -> P b c -> P a c
  SM     :: Typeable s => Name -> s -> SM s a b -> P a b

type Name = String
