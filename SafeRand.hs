module SafeRand
( pRange
, lcgLehmer
, lcgTest
) where

import Debug.Trace
debug = (flip trace) False

-- Pseudorandom orbit over the quadratic residues modulo N
qCycle :: Integral a => a -> a -> a -> a
qCycle n a x = (x^2 + a) `mod` n

-- Pseudorandom orbit over the quadratic residues modulo N
rhoCycle :: Integral a => a -> a -> a
rhoCycle n x = (x^2 - 1) `mod` n

-- A basic implementation of the Lehmer pseudo-random number generator
-- which is a linear congruential generator. Make initial call with a
-- seed value and the functions output should be fed back into its input.
lcgLehmer :: Integral a => a -> a
lcgLehmer a
  | a < 1 = lcgLehmer 1
  | otherwise = 48271*a `mod` 2147483647

--safeModT :: Integral a => a -> a -> Int
safeModT n p | trace ("safeModT " ++ show n ++ " " ++ show p) False = undefined
safeModT n p
  | 2^p > n = 2^p
  | otherwise = safeModT n $ p+1
  
--safeMod :: Integral a => a -> a
safeMod n = safeModT n 1

-- Pseudorandom numbers in the range [a,b] given seed 's'
--pRange :: Integral a => a -> a -> a -> Maybe a
pRange a b s
  | b < a  = 0
  | a == b = a
  | c >= a && c <= b = c
  | otherwise = pRange a b r
  where r = lcgLehmer s
        c = r `mod` (safeMod b)

-- A simple set of tests for determining the period of a given 
-- pseudorandom generating function f. The length of the period
-- is output. The postfix integer represents the number of
-- parameters the passed function 'f' requires.
--lcgCycle :: Integral a => (a -> a) -> a -> a -> a -> a
lcgCycle f x y t | trace ("lcgCycle f " ++ show x ++ " " ++ show y ++ " " ++ show t) False = undefined
lcgCycle f x y t =
  let a = f x
      b = f . f $ y
  in if a == b
     then t + 1
     else lcgCycle f a b $ t + 1

-- Auxilliary function for hiding counter and seed details.
--lcgTest :: Integral a => (a -> a) -> a
lcgTest f = lcgCycle f 2 2 1
