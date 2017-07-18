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
fibs :: Integer -> Integer
fibs n = iter n 0 1 where
  iter n a b = 
    if n == 0
      then a
      else iter (n-1) b $ a+b

-- Pseudorandom search for a coprime
findCoprime :: Integer -> Integer
findCoprime n = iter n . lcgLehmer $ n where
  iter n a = 
    let g = gcd n a
    in if g == 1
      then a
      else iter n . lcgLehmer $ a

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

-- rolling our own bit-length functions because Haskell's log2
-- fails on large integers

-- Fastest - log log n solution - log2 (# of bits)
-- recursivelly mask off log2(n) bits
bitlen :: Integer -> Integer
bitlen n = iter n 0 where
  iter 1 0 = 1
  iter n c
    | scale c < n   = iter n $ c+1
    | scale c == n  = 2^c+1
    | scale c > n   = 2^(c-1) + iter (div n (2^2^(c-1))) 0
    where scale f = shiftL 1 $ 2^f
    
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

-- calculating (s^2 - 2) mod m repeatedly with gigantic moduli is slow
-- fast mersenne mod trick: wikipedia.org/wiki/Lucas-Lehmer_primality_test#Time_complexity
fmmod :: Integer -> Integer -> Integer
fmmod k n
  | c <= n = k
  | g == m = 0
  | otherwise = fmmod ((k `rem` 2^n) + (k `div` 2^n)) n
  where c = bitlen k
        m = 2^n-1
        g = gcd k m
        
fmmod_bitwise :: Integer -> Integer -> Integer
fmmod_bitwise k n
  | c <= n = k
  | g == m = 0
  | otherwise = fmmod_bitwise (least + rest) n
  where c = bitlen k
        m = 2^n-1
        g = gcd k m
        rest = shiftR k (fromInteger n)
        least = xor k (shiftL rest (fromInteger n))

lucasLehmer :: Integer -> Integer -> Primality
lucasLehmer p seed
  | prime p seed == Composite = Composite
  | s == 0 = Prime
  | s /= 0 = Composite
  where
    s = iter 4 p $ p-2
    iter s p c =
      let s' = fmmod (s^2-2) p
      in if c == 0
        then s
        else iter s' p $ c-1
        
-------------------------------------------------
-- Miller-Rabin Primality Test related functions
-------------------------------------------------

-- Every even integer can be written as 2^r*d where d is odd
getRD :: Integer -> (Integer, Integer)
getRD n = iter n 1 where
  iter n r =
    let p = 2^r
        d = quot n p
    in if n `rem` p == 0 && odd d
      then (r, d)
      else iter n $ r+1
      
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
  
-- Test with:
-- map primalityMillerRabin (take 50 $ filter odd [5..])
millerRabin :: Integer -> Integer -> Primality
millerRabin p s = iter p s rounds where
  iter p s c =
    let (r,d) = getRD $ p-1
    in witnessLoop p r d s c
  
prime :: Integer -> Integer -> Primality
prime p s = if even p then Composite else millerRabin p s
