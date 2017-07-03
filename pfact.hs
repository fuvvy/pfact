-----------------------------------------------------------------------------
--
-- A small application for finding the prime factorization of a given number
-- using the very interesting Pollard's Rho algorithm. This code evolved from
-- a solution for Project Euler's third problem.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main where

import Prelude
import Data.List
import Data.Maybe
--import System.Console.CmdArgs

import SafeRand
import Primality

--import Debug.Trace
--debug = (flip trace) False

--data Options = Options {
--    num :: Integer,
--    seed :: Integer
--} deriving (Show, Data, Typeable)

--options :: Options
--options = Options {
--   num = 1 &= argPos 0 &= typ "NUMBER",
--  seed = 2 &= typ "SEED" &= name "s" &= help "Provide your own seed"
--}
--  &= summary "pfact v0.1, Finds the prime decomposition of a given number."
--  &= program "pfact"

main :: IO ()
main = do
--  Options{..} <- cmdArgs options
--  putStrLn . pretty . pfact num $ seed
  let p = 2^86243-1 in putStrLn . show . isPrime $ p

pRho :: Integer -> Integer -> Integer -> Integer -> Maybe Integer
pRho n x y c
--  | debug ("(pRho " ++ show n ++ " " ++ show x ++ " " ++ show y ++
--           ") -> a = " ++ show a ++ " b = " ++ show b) = undefined
  | a == b    = Nothing
  | p > 1     = Just p
  | otherwise = pRho n a b c  -- p == 1
  where a = q1Cycle x c n
        b = q1Cycle (q1Cycle y c n) c n
        p = gcd (abs (b - a)) n

pollardsRhoPermute :: Integer -> Integer -> Integer -> Integer
pollardsRhoPermute n s t
--  | debug ("(pollardsRhoPermute " ++ show n ++ " " ++ show s ++ " " ++ show t ++
--           ") -> c = " ++ show c ++ " p = " ++ show p) = undefined
  | t == 5 = n
  | p == Nothing = pollardsRhoPermute n c (t + 1) -- Pollard's Rho failed so permute the constant and try again
  | otherwise = fromJust p
  where c = lcgLehmer s
        p = pRho n s s c

factorize :: Integer -> Integer -> [Integer]
factorize n s
--  | debug ("*** factorize n = " ++ show n) = undefined
  | even n = [2] ++ factorize (quot n 2) s
  | n == 1 = [ ]
  | n == z = [n]
  | otherwise = factorize z s ++ factorize (quot n z) s
  where z = pollardsRhoPermute n s 1
  
pfact :: Integer -> Integer -> [Integer]
pfact n s
  | n < 2 = [n]
  | otherwise = sort . factorize n $ s

pretty' :: (Show a1, Show a, Ord a, Num a, Eq a1) => [a1] -> a -> String
pretty' [x] 1 = show x
pretty' [x] c = show x ++ "^" ++ show c
pretty' (x:y:xs) c
  | x /= y && c > 1 = show x ++ "^" ++ show c ++ " x " ++ pretty' (y:xs) 1
  | x /= y = show x ++ " x " ++ pretty' (y:xs) 1
  | otherwise = pretty' (y:xs) (c+1)

pretty :: (Show a, Eq a, Num a) => [a] -> String
pretty x
  | x == [0] || x == [1] = show x ++ " is neither prime nor composite and therefore has no prime factorization"
  | otherwise = pretty' x 1
