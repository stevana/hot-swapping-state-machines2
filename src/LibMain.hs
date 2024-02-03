module LibMain where

import Data.Typeable
import Data.IORef

import Syntax.Typed
import Syntax.Untyped
import TypeChecker
import Example.Counter
import Interpreter

------------------------------------------------------------------------

data Ref a b = Ref (IORef (T a b, [a]))

spawn :: T a b -> IO (Ref a b)
spawn f = Ref <$> newIORef (f, [])

send :: Ref a b -> a -> IO b
send (Ref r) x = do
  (f, xs) <- readIORef r
  let xs' = xs ++ [x]
  writeIORef r (f, xs')
  return (last (run f xs'))

upgrade :: Ref a b -> T a b -> IO ()
upgrade (Ref r) f' = do
  (_f, xs) <- readIORef r
  writeIORef r (f', xs)

spawn_ :: (Typeable a, Typeable b) => Ty_ -> Ty_ -> U -> IO (Ref a b)
spawn_ ua ub uf = case (inferTy ua, inferTy ub) of
  (ETy a', ETy b') -> case (cast a', cast b') of
    (Just a, Just b) -> case typeCheck uf a b of
      Right f -> spawn f
      Left err -> error ("spawn_: typecheck error: " ++ show err)
    _ -> error "spawn_: type cast error"

------------------------------------------------------------------------

main :: IO ()
main = do
  r <- spawn_ (UTEither UTUnit UTUnit) (UTEither UTInt UTUnit) tcounterU :: IO (Ref (Either () ()) (Either Int ()))
  _ <- send r incrCount
  c <- send r readCount
  print c
  _ <- send r incrCount
  _ <- send r incrCount
  c' <- send r readCount
  print c'

-- >>> main
-- Left 1
-- Left 3
