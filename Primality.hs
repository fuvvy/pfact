module Primality (
  llt,
  mrt,
  Primality(..) ) where

import Prelude
import Data.Bits

import SafeRand

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

pow2 :: Integer -> Integer
pow2 n = shiftL 1 (fromInteger n)

-- Get most significant bits b of integer n
msb :: Integer -> Integer -> Integer
msb n b = shiftR n (fromInteger ((bitlen n)-b))

-- Get least significant bits b of integer n
lsb :: Integer -> Integer -> Integer
lsb n b = (toInteger . fromInteger) ((.&.) n ((shiftL 1 (fromInteger b)-1)))

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

-- Calculating (s^2 - 2) mod m repeatedly with gigantic moduli is slow
-- For m = 2^n-1, use fmmod k n
-- Edge case: multiple of modulus should produce 0 and not 2^n-1
-- Assert fmmod 49146 13 = 0, not 8191
-- fast mersenne mod trick: wikipedia.org/wiki/Lucas-Lehmer_primality_test#Time_complexity

fmmod :: Integer -> Integer -> Integer -> Integer
fmmod k n m
  | k == m = 0
  | c <= n = k
  | otherwise = fmmod (lbits + hbits) n m
  where c = bitlen k
        lbits = lsb k n
        hbits = msb k $ c-n

-- For reference. Same as above, without any bit-shifting
fmmod_ref :: Integer -> Integer -> Integer
fmmod_ref k n
  | k == m = 0
  | c <= n = k
  | otherwise = fmmod_ref ((k `rem` 2^n) + (k `div` 2^n)) n
  where c = bitlen k
        m = (pow2 n)-1

-- llt - Lucas-Lehmer Test
llt :: Integer -> Integer -> Primality
llt p seed
  | mrt p seed == Composite = Composite
  | s == 0 = Prime
  | s /= 0 = Composite
  where
    s = iter 4 p $ p-2
    m = (pow2 p)-1
    iter s p c =
      let s' = fmmod (s^2-2) p m
      in if c == 0
        then s
        else iter s' p $ c-1
        
-------------------------------------------------
-- Miller-Rabin Primality Test logic
-------------------------------------------------
  
rounds :: Integer -> Integer
rounds = ceiling . logBase 4 . fromInteger
  
-- mrt - Miller-Rabin Test
-- Test with: map mrt (take 50 $ filter odd [5..])
mrt :: Integer -> Integer -> Primality
mrt p s = iter p s $ rounds p where
  iter p s c =
    if even p
    then Composite
    else let (r,d) = rd $ p-1 in witnessLoop p r d s c
    where
      -- Every even integer can be written as 2^r*d where d is odd
      rd n = iter n 1 where
        iter n r =
          let p = 2^r
              d = quot n p
          in if n `rem` p == 0 && odd d
            then (r, d)
            else iter n $ r+1
      -- Search for composite witnesses 
      witnessLoop n r d s c
        | c == 0                              = ProbablyPrime
        | x == 1 || x == n-1 || l == Continue = (witnessLoop n r d) t $ c-1
        | otherwise                           = Composite
        where
          t = lcgLehmer s
          a = safeRange 2 (n-2) s
          x = modExp a d n
          l = rloop n x $ r-1
          rloop n x c
            | c == 0 || y == 1  = Composite
            | y == n-1          = Continue
            | otherwise         = rloop n y $ c-1
            where y = modExp x 2 n
