module IntegerUtilities (
  pow2,
  msb,
  lsb,
  splitAtBit,
  splitAtDigit,
  base10len,
  bitlen,
) where

import Prelude
import Data.Bits
import Math.NumberTheory.Logarithms

pow2 :: Integer -> Integer
pow2 n = shiftL 1 (fromInteger n)

-- Get most significant bits b of integer n
msb :: Integer -> Integer -> Integer
msb n b = shiftR n (fromInteger ((bitlen n)-b))

-- Get least significant bits b of integer n
lsb :: Integer -> Integer -> Integer
lsb n b = (toInteger . fromInteger) ((.&.) n ((shiftL 1 (fromInteger b)-1)))

-- Split an integer 'n' into high and low bits at [.. b+1],[b ..]
splitAtBit :: Integer -> Integer -> (Integer, Integer)
splitAtBit n b =
  let l = lsb n b
      h = msb n $ b+1
  in (h, l)
  
splitAtDigit :: Integer -> Integer -> (Integer, Integer)
splitAtDigit n d =
  let l = rem n $ 10^d
      h = quot n $ 10^d
  in (h, l)
  
base10len :: Integer -> Integer
base10len n = (+1) $ (toInteger (integerLog10 n))

-- rolling our own bit-length functions because Haskell's log2
-- fails on large integers

-- Fastest - log log n solution - log2 (# of bits)
-- Recursivelly reach ahead 2^2^i until overshoot, count 2^2^(i-1),
-- continue recursively 2^2^(i=0) with new # minus the first
-- counted 2^2^(i-1) bits
bitlen :: Integer -> Integer
bitlen n = iter n 0 where
  iter 1 0 = 1
  iter n c
    | scaled < n  = iter n $ c+1
    | scaled == n = (+1) . pow2 $ c
    | scaled > n  = lsb_count + iter msb_left 0
    where
      scale f   = pow2 . pow2 $ f
      scaled    = scale c
      lsb_count = pow2 $ c-1
      msb_left  = shiftR n . fromInteger . pow2 $ c-1
      
-- Math.NumberTheory.Logarithms.integerLog2 solution no faster than above
bitlen_ilog :: Integer -> Integer
bitlen_ilog = toInteger . (+1) . integerLog2
    
-- Very fast - same as bitlen but with no bit shifting
bitlen_pow :: Integer -> Integer
bitlen_pow n = iter n 0 where
  iter 1 0 = 1
  iter n c
    | scale c < n   = iter n $ c+1
    | scale c == n  = 2^c+1
    | scale c > n   = 2^(c-1) + iter (div n (2^2^(c-1))) 0
    where scale f = 2^2^f
    
-- Fast - test n against 2^i until overshoot then linear drift down
bitlen_drift :: Integer -> Integer
bitlen_drift n = up n 0 where
  down n c =
    if (shiftL 1 (fromIntegral c)) > n
      then down n $ c-1
      else c+1
  up n c =
    if (shiftL 1 (2^c)) < n
      then up n $ c+1
      else down n $ 2^c

-- Middle of the road - linear in number of bits
bitlen_lin :: Integer -> Integer
bitlen_lin n = iter n 1 where
  iter n a
    | b == n = a+1
    | b >  n = a
    | b <  n = iter n $ a+1
    where b = shiftL 1 (fromIntegral a)

--Slow
bitlen_div :: Integer -> Integer
bitlen_div n = (iter 2 n) + 1 where
  iter a b
    | b == 1    = 0
    | otherwise = 1 + iter a (b `div` a)

--Slowest
bitlen_exp :: Integer -> Integer
bitlen_exp n = iter n 1 where
  iter n a
    | b == n = a+1
    | b >  n = a
    | b <  n = iter n $ a+1
    where b = 2^a
