module Primality ( prime, Primality(..) ) where

import Prelude
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
        
lucasLehmer' :: Integer -> Integer -> Integer -> Integer
lucasLehmer' s m c =
  let s' = (s^2 - 2) `rem` m
  in if c == 0
    then s
    else lucasLehmer' s' m $ c-1

lucasLehmer :: Integer -> Integer -> Primality
lucasLehmer p seed
  | prime p seed == Composite = Composite
  | s == 0 = Prime
  | s /= 0 = Composite
  where m = 2^p-1
        s = lucasLehmer' 4 m $ p-2
        
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
