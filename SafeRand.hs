module SafeRand (
  lcgLehmer,
  modExp,
  safeRange,
  riskRange
  ) where
  
import Data.Bits

--import Debug.Trace
--debug = (flip trace) False

-- A basic implementation of the Lehmer pseudo-random number generator
-- which is a linear congruential generator. Make initial call with a
-- seed value and the functions output should be fed back into its input.
lcgLehmer :: Integer -> Integer
lcgLehmer a
  | a < 1 = lcgLehmer 1
  | otherwise = 48271*a `mod` 2147483647

safeMod' :: Integer -> Integer -> Integer
--safeMod' n p | trace ("safeMod' " ++ show n ++ " " ++ show p) False = undefined
safeMod' n p
  | 2^p > n = 2^p
  | otherwise = safeMod' n $ p+1
  
-- Safe modulus for picking random integers without bias
safeMod :: Integer -> Integer
safeMod n = safeMod' n 1

-- Fast modular exponentiation
modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m  = 1
modExp b e m  = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
              where t = if testBit e 0 then b `mod` m else 1

-- Pseudorandom numbers in the range [a,b] given seed 's'
-- Keep picking until one is found within the desired range
safeRange :: Integer -> Integer -> Integer -> Integer
safeRange a b s
  | b < a  = 0
  | a == b = a
  | c >= a && c <= b = c
  | otherwise = safeRange a b r
  where r = lcgLehmer s
        c = r `mod` (safeMod b)
        
-- Pseudorandom numbers in the range [a,b] given seed 's'
-- Map seed into the desired range using a modulus
-- Could introduce bias
riskRange :: Integer -> Integer -> Integer -> Integer
riskRange a b s =
  let n = b - a + 1
      i = s `mod` n
  in a + i

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
