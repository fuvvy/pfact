{-# LANGUAGE MagicHash #-}
module Primality ( prime, lucasLehmer, Primality(..) ) where

import Prelude
import Data.Bits

import SafeRand

rounds = 1
data Primality = Composite | ProbablyPrime | Prime | Continue deriving (Show, Eq, Enum)

----------------------------------------
-- PSW Primality Test related functions
----------------------------------------

-- Fast, tail-recursive Fibonacci function
fibs' :: Integer -> Integer -> Integer -> Integer
fibs' n a b = 
  if n == 0
    then a
    else fibs' (n-1) b $ a+b

fibs :: Integer -> Integer
fibs n = fibs' n 0 1

-- Pseudorandom search for a coprime
findCoprime' :: Integer -> Integer -> Integer
findCoprime' n a = 
  let g = gcd n a
  in if g == 1
    then a
    else findCoprime' n . lcgLehmer $ a

findCoprime :: Integer -> Integer
findCoprime n = findCoprime' n . lcgLehmer $ n

-- Fermat primality test
-- 2^(p-1) = 1 (mod p)
fermatTest :: Integer -> Bool
fermatTest n =
  let a = findCoprime n
      r = modExp a (n-1) n
  in if r == 1
    then True
    else False
    
-- Fibonacci primality test
-- fibs(p+1) = 0 (mod p)
fibsTest :: Integer -> Bool
fibsTest p =
  let q = fibs p+1
      r = q `rem` p
  in if r == 0
    then True
    else False
    
-- PSW test is a combination of Fermat and Fibs tests
pswTest :: Integer -> Bool
pswTest p
  | e && f = True
  | otherwise = False
  where e = fermatTest p
        f = fibsTest p
        
-------------------------------------------------
-- Lucas-Lehmer Mersenne primality test
-------------------------------------------------

-- rolling our own integer log2 because Haskell's sucks
bitlen' :: Integer -> Integer -> Integer
bitlen' n a
  | b == n = a+1
  | b >  n = a
  | b <  n = bitlen' n $ a+1
  where b = 2^a

bitlen :: Integer -> Integer
bitlen n = bitlen' n 1

-- calculating (s^2 - 2) mod m repeatedly with gigantic moduli is slow
-- fast mod trick: wikipedia.org/wiki/Lucas-Lehmer_primality_test#Time_complexity
fastMersenneMod :: Integer -> Integer -> Integer
fastMersenneMod k n
  | c <= n = k
  | g == m = 0
  | otherwise = fastMersenneMod ((k `rem` 2^n) + (k `div` 2^n)) n
  where c = bitlen k
        m = 2^n-1
        g = gcd k m
        
fastMersenneMod2 :: Integer -> Integer -> Integer
fastMersenneMod2 k n
  | c <= n = k
  | g == m = 0
  | otherwise = fastMersenneMod2 (least + rest) n
  where c = bitlen k
        m = 2^n-1
        g = gcd k m
        rest = shiftR k (fromInteger n)
        least = xor k (shiftL rest (fromInteger n))
        
lucasLehmer' :: Integer -> Integer -> Integer -> Integer
lucasLehmer' s p c =
  let s' = fastMersenneMod (s^2-2) p
  in if c == 0
    then s
    else lucasLehmer' s' p $ c-1

lucasLehmer :: Integer -> Integer -> Primality
lucasLehmer p seed
  | prime p seed == Composite = Composite
  | s == 0 = Prime
  | s /= 0 = Composite
  where s = lucasLehmer' 4 p $ p-2
        
-------------------------------------------------
-- Miller-Rabin Primality Test related functions
-------------------------------------------------

-- Every even integer can be written as 2^r*d where d is odd
getRD' :: Integer -> Integer -> (Integer, Integer)
getRD' n r =
  let p = 2^r
      d = quot n p
  in if n `rem` p == 0 && odd d
    then (r, d)
    else getRD' n $ r+1

getRD :: Integer -> (Integer, Integer)
getRD n = getRD' n 1
      
innerLoop :: Integer -> Integer -> Integer -> Primality
innerLoop n x c
  | c == 0 || y == 1  = Composite
  | y == n-1          = Continue
  | otherwise         = innerLoop n y $ c-1
  where y = modExp x 2 n
  
witnessLoop :: Integer -> Integer -> Integer -> Integer -> Integer -> Primality
witnessLoop n r d s c
  | c == 0                              = ProbablyPrime
  | x == 1 || x == n-1 || l == Continue = (witnessLoop n r d) t $ c-1
  | otherwise                           = Composite
  where t = lcgLehmer s
        a = safeRange 2 (n-2) t
        x = modExp a d n
        l = innerLoop n x $ r-1

millerRabin' :: Integer -> Integer -> Integer -> Primality
millerRabin' p s c =
  let (r,d) = getRD $ p-1
  in witnessLoop p r d s c
  
-- Test with:
-- map primalityMillerRabin (take 50 $ filter odd [5..])
millerRabin :: Integer -> Integer -> Primality
millerRabin p s = millerRabin' p s rounds
  
prime :: Integer -> Integer -> Primality
prime p s = if even p then Composite else millerRabin p s
