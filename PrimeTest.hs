module PrimeTest ( primalityFermat, primalityMillerRabin ) where

import Data.Maybe
--import Prelude
--import Debug.Trace

import SafeRand

--debug = (flip trace) False

data Primality = Composite | ProbablyPrime | Continue deriving (Show, Eq, Enum)

fibsT n a b = 
  if n == 0
    then a
    else fibsT (n-1) b (a+b)

fibs n = fibsT n 0 1

findCoprimeR :: Integral a => a -> a -> a
findCoprimeR n a = 
  let g = gcd n a
  in if g == 1
    then a
    else findCoprimeR n . lcgLehmer $ a

findCoprime :: Integral a => a -> a
findCoprime n = findCoprimeR n . lcgLehmer $ n

primalityFermat :: Integral t => t -> Bool
primalityFermat n =
  let a = findCoprime n
      r = a^(n-1) `mod` n
  in if r == 1
    then True
    else False
    
primalityFibonacci :: Integral t => t -> Bool
primalityFibonacci p =
  let q = fibs p+1
      r = q `mod` p
  in if r == 0
    then True
    else False
    
primalityPSW :: Integral t => t -> Bool
primalityPSW p
  | e && f = True
  | otherwise = False
  where e = primalityFermat p
        f = primalityFibonacci p
        
isPrime :: Integral t => t -> Bool
isPrime p
  | o == False = False
  | o && r == 2 && t = True
  | otherwise = False
  where o = odd p
        r = p `rem` 5
        t = primalityPSW p

getRD :: Integral t => t -> (t, t)
getRD n = getRDT (n-1) 1

getRDT :: Integral t => t -> t -> (t, t)
getRDT n r =
  let p = 2^r
      d = quot n p
  in if n `rem` p == 0 && odd d
    then (r, d)
    else getRDT n (r+1)

pickRangeInclusive :: Integral t => t -> t -> t -> t
pickRangeInclusive a b s =
  let n = b - a + 1
      i = s `mod` n
  in a + i
  
-- Test with:
-- map primalityMillerRabin (take 50 $ filter odd [5..])
primalityMillerRabin p = primalityMillerRabinR p 1337 10

--primalityMillerRabinR :: Int -> Bool
primalityMillerRabinR p s k =
  let (r,d) = getRD p
  in witnessLoop p r d s k

--witnessLoop :: Integral t => t -> t -> t -> t -> t -> Bool
witnessLoop n r d s k
--  | debug ("witnessLoop " ++ show n ++ " " ++ show r ++ " " ++ show d ++ " " ++ show s ++ " " ++ show k) = False
--  | k == 0              = ProbablyPrime
--  | x == 1 || x == n-1  = witnessLoop n r d t (k-1)
--  | l == Composite      = Composite
--  | l == Continue       = witnessLoop n r d t (k-1)
  | k == 0                              = ProbablyPrime
  | x == 1 || x == n-1 || l == Continue = witnessLoop n r d t (k-1)
  | l == Composite                      = Composite
  where a = pickRangeInclusive 2 (n-2) s
        x = a^d `mod` n
        t = lcgLehmer s
        l = innerLoop n x (r-1)
      
--innerLoop :: Integral t => t -> t -> t -> t
innerLoop n x r
--  | debug ("innerLoop " ++ show n ++ " " ++ show x ++ " " ++ show r) = undefined
  | r == 0 || y == 1  = Composite
  | y == n-1          = Continue
  | otherwise         = innerLoop n y (r-1)
  where y = x^2 `mod` n
  
--rangeTest :: Integral t => t -> t -> String
rangeTest s t
  | t == 0 = -1*s
  | p < 2 || p > 100 = s
  | otherwise = rangeTest (lcgLehmer s) (t-1)
  where p = pickRangeInclusive 2 100 s
