module Stream where

------------------------------------------------------------------------
-- * Streams

data Stream a = Cons a (Stream a)
  deriving Show

mapS :: (a -> b) -> Stream a -> Stream b
mapS f ~(Cons x xs) = Cons (f x) (mapS f xs)

unzipS :: Stream (a, b) -> (Stream a, Stream b)
unzipS ~(Cons (x, y) xys) = (Cons x (fst (unzipS xys)), Cons y (snd (unzipS xys)))

-- https://hackage.haskell.org/package/contravariant-1.5.5/docs/Data-Functor-Contravariant-Divisible.html
zipS :: (Stream a, Stream b) -> Stream (a, b)
zipS (~(Cons x xs), ~(Cons y ys)) = Cons (x, y) (zipS (xs, ys))

splitS :: Stream (Either a b) -> (Stream a, Stream b)
splitS xys0 = (goL xys0, goR xys0)
  where
    goL (Cons (Left  x)  xys) = Cons x (goL xys)
    goL (Cons (Right _y) xys) = goL xys

    goR (Cons (Left  _x) xys) = goR xys
    goR (Cons (Right y)  xys) = Cons y (goR xys)

combineS :: Stream (Either x y) -> (Stream a, Stream b) -> Stream (Either a b)
combineS (Cons (Left  _) es) (Cons x xs, ys) = Cons (Left  x) (combineS es (xs, ys))
combineS (Cons (Right _) es) (xs, Cons y ys) = Cons (Right y) (combineS es (xs, ys))

scycle :: [a] -> Stream a
scycle []  = error "scycle"
scycle xs0 = go xs0
  where
    go []       = go xs0
    go (x : xs) = Cons x (go xs)

stake :: Int -> Stream a -> [a]
stake 0 _           = []
stake n (Cons x xs) = x : stake (n - 1) xs

toListS :: Stream a -> [a]
toListS (Cons x xs) = x : toListS xs

fromListS :: [a] -> Stream a
fromListS (x:xs) = Cons x (fromListS xs)
fromListS []     = error "fromListS: finite list"

constS :: a -> Stream a
constS x = Cons x (constS x)

instance Num a => Num (Stream a) where
  Cons m ms + Cons n ns = Cons (m + n) (ms + ns)
  Cons m ms - Cons n ns = Cons (m - n) (ms - ns)
  Cons m ms * Cons n ns = Cons (m * n) (ms * ns)
  negate (Cons n ns) = Cons (negate n) (negate ns)
  abs (Cons n ns) = Cons (abs n) (abs ns)
  signum (Cons n ns) = Cons (signum n) (signum ns)
  fromInteger n = constS (fromInteger n)
