module Classes where

import Control.Category

------------------------------------------------------------------------

class Category k => Cartesian k where
  copy    :: k a (a, a)
  consume :: k a ()
  proj1   :: k (a, b) a
  proj2   :: k (a, b) b

class Category k => Cocartesian k where
  inj1 :: k a (Either a b)
  inj2 :: k b (Either a b)
  split :: k a c -> k b c -> k (Either a b) c

-- https://hackage.haskell.org/package/profunctors

class Category k => Strong k where
  first'  :: k a b -> k (a, c) (b, c)
  second' :: k b c -> k (a, b) (a, c)

-- Analogous to ArrowLoop, where loop = unfirst.
class Costrong k where
  unfirst :: k (a, c) (b, c) -> k a b

class Choice k where
  left'  :: k a b -> k (Either a c) (Either b c)
  right' :: k a b -> k (Either c a) (Either c b)

class Cochoice k where
  unleft :: k (Either a c) (Either b c) -> k a b
