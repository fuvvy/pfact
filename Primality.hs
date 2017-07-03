module Primality ( isPrime ) where

import Prelude
--import Debug.Trace

import SafeRand

--debug = (flip trace) False

data Primality = Composite | ProbablyPrime | Continue deriving (Show, Eq, Enum)

----------------------------------------
-- PSW Primality Test related functions
----------------------------------------

-- Fast, tail-recursive Fibonacci function
fibsT :: Integer -> Integer -> Integer -> Integer
fibsT n a b = 
  if n == 0
    then a
    else fibsT (n-1) b (a+b)

fibs :: Integer -> Integer
fibs n = fibsT n 0 1

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
      r = q `mod` p
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
innerLoop n x r
--  | debug ("innerLoop " ++ show n ++ " " ++ show x ++ " " ++ show r) = undefined
  | r == 0 || y == 1  = Composite
  | y == n-1          = Continue
  | otherwise         = innerLoop n y (r-1)
  where y = modExp x 2 n
  
witnessLoop :: Integer -> Integer -> Integer -> Integer -> Integer -> Primality
witnessLoop n r d s k
--  | debug ("witnessLoop " ++ show n ++ " " ++ show r ++ " " ++ show d ++ " " ++ show s ++ " " ++ show k) = False
  | k == 0                              = ProbablyPrime
  | x == 1 || x == n-1 || l == Continue = (witnessLoop n r d) t $ k-1
  | otherwise                           = Composite
  where t = lcgLehmer s
        a = safeRange 2 (n-2) t
        x = modExp a d n
        l = innerLoop n x $ r-1

millerRabin' :: Integer -> Integer -> Integer -> Primality
millerRabin' p s k =
  let (r,d) = getRD $ p-1
  in witnessLoop p r d s k
  
-- Test with:
-- map primalityMillerRabin (take 50 $ filter odd [5..])
millerRabin p = millerRabin' p (lcgLehmer p) 1
  
isPrime :: Integer -> Primality
isPrime p = if even p then Composite else millerRabin p
