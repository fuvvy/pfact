module PrimeTest ( testFermat, odd ) where

import Data.Maybe
import Prelude
import Debug.Trace

import SafeRand

debug = (flip trace) False

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

testFermat :: Integral t => t -> Bool
testFermat n =
  let a = findCoprime n
      r = a^(n-1) `mod` n
  in if r == 1
    then True
    else False
    
testFibonacci :: Integral t => t -> Bool
testFibonacci p =
  let q = fibs p+1
      r = q `mod` p
  in if r == 0
    then True
    else False
    
testPSW :: Integral t => t -> Bool
testPSW p
  | e && f = True
  | otherwise = False
  where e = testFermat p
        f = testFibonacci p
        
isPrime :: Integral t => t -> Bool
isPrime p
  | o == False = False
  | o && r == 2 && t = True
  | otherwise = False
  where o = odd p
        r = p `rem` 5
        t = testPSW p

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
-- map testMillerRabin (take 50 $ filter odd [5..])
testMillerRabin p = testMillerRabinR p 1337 10

--testMillerRabinR :: Int -> Bool
testMillerRabinR p s k =
  let (r,d) = getRD p
      l = witnessLoop p r d s k
  in if l == False
    then False
    else True

--witnessLoop :: Integral t => t -> t -> t -> t -> t -> Bool
witnessLoop n r d s k
--  | debug ("witnessLoop " ++ show n ++ " " ++ show r ++ " " ++ show d ++ " " ++ show s ++ " " ++ show k) = False
  | k == 0              = True
  | x == 1 || x == n-1  = witnessLoop n r d (lcgLehmer s) (k-1)
  | l == 0              = False
  | l == 2              = False
  | l == 1              = witnessLoop n r d (lcgLehmer s) (k-1)
  | l == n-1            = witnessLoop n r d (lcgLehmer s) (k-1)
  where a = pickRangeInclusive 2 (n-2) s
        x = a^d `mod` n
        l = innerLoop n x (r-1)
      
--innerLoop :: Integral t => t -> t -> t -> t
innerLoop n x r
--  | debug ("innerLoop " ++ show n ++ " " ++ show x ++ " " ++ show r) = undefined
  | y == 1    = 0
  | y == n-1  = 1
  | r == 0    = 2
  | otherwise = innerLoop n y (r-1)
  where y = x^2 `mod` n
  
--rangeTest :: Integral t => t -> t -> String
rangeTest s t
  | t == 0 = -1*s
  | p < 2 || p > 100 = s
  | otherwise = rangeTest (lcgLehmer s) (t-1)
  where p = pickRangeInclusive 2 100 s
