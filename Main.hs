-----------------------------------------------------------------------------
--
-- A small application for finding the prime factorization of a given number
-- using the very interesting Pollard's Rho algorithm. This code evolved from
-- a solution for Project Euler's third problem.
--
-----------------------------------------------------------------------------
--{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs
import PFact
import Primality

data Putil  = Factor {integer :: Integer, seed :: Integer}
            | Mrt {integer :: Integer, seed :: Integer}
            | Llt {integer :: Integer, seed :: Integer}  
            deriving (Show, Data, Typeable, Eq)

{-
options :: Options
options = Options {
  num = 1 &= argPos 0 &= typ "NUMBER",
  seed = 2 &= typ "SEED" &= name "s" &= help "Provide your own seed"
}
  &= summary "pfact v0.1, Finds the prime decomposition of a given number."
  &= program "pfact"
-}
  
factor = Factor {
  integer = def &= typ "NUM" &= name "n" &= argPos 0,
  seed = def &= typ "SEED" &= name "s" &= help "Provide your own seed" &= argPos 1
} &= help "Find prime factors"
  
mrtest = Mrt {
  integer = def &= typ "NUM" &= name "n" &= argPos 0,
  seed = def &= typ "SEED" &= name "s" &= help "Provide your own seed" &= argPos 1
} &= help "Test for primality using Miller-Rabin"

lltest = Llt {
  integer = def &= typ "NUM" &= name "e" &= help "Mersenne exponent" &= argPos 0,
  seed = def &= typ "SEED" &= name "s" &= help "Provide your own seed" &= argPos 1
} &= help "Test mersenne number for primality using Lucas-Lehmer"

mode  = cmdArgsMode $ modes [factor,mrtest,lltest]
     &= help "Prime Utility"
     &= program "putil"
     &= summary "putil v1.0\nPrime testing and factorization"

pretty :: (Show a, Eq a, Num a) => [a] -> String
pretty x
  | x == [0] || x == [1] = show x ++ " is neither prime nor composite and therefore has no prime factorization"
  | otherwise = iter x 1
  where
    iter [x] 1 = show x
    iter [x] c = show x ++ "^" ++ show c
    iter (x:y:xs) c
      | x /= y && c > 1 = show x ++ "^" ++ show c ++ " x " ++ iter (y:xs) 1
      | x /= y = show x ++ " x " ++ iter (y:xs) 1
      | otherwise = iter (y:xs) $ c+1

main :: IO ()
main = do
  x <- cmdArgsRun mode
 -- Options{..} <- cmdArgs options
--  Options{..} <- cmdArgs mode
--  putStrLn . pretty . pfact num $ seed
--  putStrLn . show . llt 1257787 $ 5489439
  putStrLn . show . llt 44497 $ 5489439
