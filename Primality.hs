module Primality (
  llt,
  mrt,
  searchPrimes,
  Primality(..) ) where

import Math.NumberTheory.Logarithms

import IntegerUtils
import RandomUtils

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
llt :: Integer -> Primality
llt p
  | mrt p == Composite = Composite
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
rounds = toInteger . integerLogBase 4

-- Every even integer can be written as 2^r*d where d is odd
getrd :: Integer -> (Integer, Integer)
getrd n = iter n 1 where
   iter n r =
      let p = pow2 r
          d = quot n p
      in if n `rem` p == 0 && odd d
         then (r,d)
         else iter n $ r+1
  
-- mrt - Miller-Rabin Test
-- Test with: map mrt (take 50 $ filter odd [5..])
mrt :: Integer -> Primality
mrt p = iter p $ rounds p where
  iter p c =
    if even p
    then Composite
    else let (r,d) = getrd $ p-1 in witnessLoop p r d c
    where
      -- Search for composite witnesses
      witnessLoop n r d c
        | c == 0                              = ProbablyPrime
        | x == 1 || x == n-1 || l == Continue = witnessLoop n r d $ c-1
        | otherwise                           = Composite
        where
          a = getRandRange 2 $ n-2
          x = modExp a d n
          l = rloop n x $ r-1
          rloop n x c
            | c == 0 || y == 1  = Composite
            | y == n-1          = Continue
            | otherwise         = rloop n y $ c-1
            where y = modExp x 2 n

-------------------------------------------------
-- Prime search logic
-------------------------------------------------

-- Given integer n, produce a 'primality tuple' consisting of the primality
-- of integer n and the marked integer n
markPrimality :: Integer -> (Primality, Integer)
markPrimality n = (mrt n, n)

-- Given bits b, return an infinite list of primality tuples where each
-- integer is pseudo-random and of bit-length b
generatePrimalityStream :: Integer -> [(Primality, Integer)]
generatePrimalityStream b = map markPrimality . filter odd $ getRandBitStream b

-- Search through an infinte list of primality tuples and return a list of length
-- n consisting of prime numbers of bit-length b
searchPrimalityStream :: Integer -> Integer -> [(Primality, Integer)] -> [Integer]
searchPrimalityStream b n ((p,i):xs)
   | n == 0 = []
   | p == Composite = searchPrimalityStream b n xs
   | p == ProbablyPrime = i : searchPrimalityStream b (n-1) xs

-- Pseudo-random search through odd integers of bit-length b and return a list of
-- probable primes of length n
searchPrimes :: Integer -> Integer -> [Integer]
searchPrimes b n = searchPrimalityStream b n $ generatePrimalityStream b
