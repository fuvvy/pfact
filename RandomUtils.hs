module RandomUtils (
  lcgLehmer,
  modExp,
  riskRange,
  getRandRange,
  getRandBits,
  getRandStream,
  getRandBitStream,
  ) where
  
import Data.Bits
import System.Random
import System.IO.Unsafe

import IntegerUtils

-- A basic implementation of the Lehmer pseudo-random number generator
-- which is a linear congruential generator. Make initial call with a
-- seed value and the functions output should be fed back into its input.
lcgLehmer :: Integer -> Integer
lcgLehmer a
  | a < 1 = lcgLehmer 1
  | otherwise = 48271*a `rem` 2147483647

-- Fast modular exponentiation
modExp :: Integer -> Integer -> Integer -> Integer
modExp b e m
  | e == 0    = 1
  | otherwise = t * modExp ((b * b) `rem` m) (shiftR e 1) m `rem` m
  where t = if testBit e 0 then b `rem` m else 1

-- Poll the pseudo-random generator for a integer in the range [a..b]
getRandRange :: Integer -> Integer -> Integer
getRandRange a b = unsafePerformIO . getStdRandom $ randomR (a, b) :: Integer

-- Get a pseudo-random integer of bit length b
-- i.e. a number in the range 2^(b-1) through 2^b-1
getRandBits :: Integer -> Integer
getRandBits b = getRandRange (pow2 (b-1)) ((pow2 b)-1)

-- Infinite list of pseudo-random integers in the range [a..b]
getRandStream :: Integer -> Integer -> [Integer]
getRandStream a b = getRandRange a b : getRandStream a b

-- Infinte list of pseudo-random integers of bit length b
getRandBitStream :: Integer -> [Integer]
getRandBitStream b = getRandBits b : getRandBitStream b
        
-- Pseudorandom numbers in the range [a,b]
-- Could introduce bias
riskRange :: Integer -> Integer -> Integer
riskRange a b =
  let n = b - a + 1
      s = getRandBits $ bitlen b
      i = s `rem` n
  in a + i

-- Safe modulus for picking random integers without bias
safeMod :: Integer -> Integer
safeMod n = pow2 . (+1) $ bitlen n

-- Pseudorandom numbers in the range [a,b]
-- Keep picking until one is found within the desired range
safeRange :: Integer -> Integer -> Integer
safeRange a b
  | b < a  = 0
  | a == b = a
  | c >= a && c <= b = c
  | otherwise = safeRange a b
  where r = getRandBits $ safeMod b
        c = r `rem` (safeMod b)

-- A simple set of tests for determining the period of a given 
-- pseudorandom generating function f. The length of the period
-- is output. The postfix integer represents the number of
-- parameters the passed function 'f' requires.
lcgCycle :: (Integer -> Integer) -> Integer -> Integer -> Integer -> Integer
lcgCycle f x y t =
  let a = f x
      b = f . f $ y
  in if a == b
     then t+1
     else lcgCycle f a b $ t+1

-- Auxilliary function for hiding counter and seed details.
lcgTest :: (Integer -> Integer) -> Integer
lcgTest f = lcgCycle f 2 2 1
