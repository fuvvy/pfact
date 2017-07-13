-----------------------------------------------------------------------------
--
-- A small application for finding the prime factorization of a given number
-- using the very interesting Pollard's Rho algorithm. This code evolved from
-- a solution for Project Euler's third problem.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main where

import System.Console.CmdArgs
import PFact
import Primality

data Options = Options {
    num :: Integer,
    seed :: Integer
} deriving (Show, Data, Typeable)

options :: Options
options = Options {
   num = 1 &= argPos 0 &= typ "NUMBER",
  seed = 2 &= typ "SEED" &= name "s" &= help "Provide your own seed"
}
  &= summary "pfact v0.1, Finds the prime decomposition of a given number."
  &= program "pfact"

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

main :: IO ()
main = do
--  Options{..} <- cmdArgs options
--putStrLn . pretty . pfact num $ seed
--putStrLn . show . lucasLehmer 110503 $ 5489439
  putStrLn . show . lucasLehmer 1279 $ 5489439
--  let p = 2^110503-1 in putStrLn . show . prime p $ 5489439
