module FileSource where

import Data.Bits
import Data.Char
import Data.Word
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Ptr

------------------------------------------------------------------------

split :: Int -> String -> [String]
split n = go []
  where
    go acc "" = reverse acc
    go acc s  = go (take n s : acc) (drop n s)

nl :: [String] -> [String]
nl = go
  where
    go [] = []
    go ("" : xs) = go xs
    go (x : xs) | last x == '\n' = x : go xs
                | otherwise = let (x', xs') = go' xs in (x ++ x') : go xs'
    go' :: [String] -> (String, [String])
    go' [] = ("", [])
    go' (y : ys) = case break (== '\n') y of
      (y', '\n' : rest) -> (y', rest : ys)
      (y', []) -> let (z, r) = go' ys in (y' ++ z, r)

prop_split :: Int -> String -> Bool
prop_split n s | n > 0 = unlines (nl (split n s)) == s

------------------------------------------------------------------------

mmap :: FilePath -> IO (Ptr a)
mmap = undefined

nextNewline :: Ptr Word64 -> IO (Ptr Word64)
nextNewline ptr = do
  u64 <- peek ptr :: IO Word64
  let input = u64 `xor` 0x0A0A0A0A0A0A0A0A
  -- https://graphics.stanford.edu/~seander/bithacks.html#ZeroInWord
  let pos :: Word64
      pos = (input - 0x0101010101010101) .&. (complement input .&. 0x8080808080808080)
  if pos == 0
  then nextNewline (ptr `plusPtr` 8)
  else return (ptr `plusPtr` (countTrailingZeros pos `shiftR` 3))

testp :: IO ()
testp = alloca $ \(ptr :: Ptr Word8) -> do
  pokeByteOff ptr 0 'a'
  pokeByteOff ptr 1 'p'
  pokeByteOff ptr 2 'a'
  pokeByteOff ptr 3 '\n'
  pokeByteOff ptr 4 'b'
  pokeByteOff ptr 5 'e'
  pokeByteOff ptr 6 'p'
  pokeByteOff ptr 7 'a'
  ptr' <- nextNewline (castPtr ptr)
  nl <- peek (castPtr ptr') :: IO Word8
  b <- peekByteOff (castPtr ptr') 1 :: IO Word8
  print (chr (fromIntegral nl))
  print (chr (fromIntegral b))
