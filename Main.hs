{------------------------------------------------------------------------------
--
-- A small application for testing primality and finding prime factorizations
-- This code evolved from a solution for Project Euler's third problem.
--
-------------------------------------------------------------------------------}
{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main where

import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
import System.Exit
import PFact
import Primality

_PROGRAM_NAME     = "putil"
_PROGRAM_VERSION  = "1.0"
_PROGRAM_INFO     = _PROGRAM_NAME ++ " v" ++ _PROGRAM_VERSION ++ ", " ++ _AUTHOR
_PROGRAM_DESC     = "Primality testing and prime factorization utility"
_PROGRAM_SUMMARY  =  _PROGRAM_INFO ++ "\n" ++ _PROGRAM_DESC
_PROGRAM_USAGE    = "Examples:\n\tputil fac 3895462145894328\n\tputil mrt 18848997157\n\tputil llt 110503\n\tputil fpp 1024 3"
_AUTHOR           = "(A) github.com/fuvvy"

data Putil  = Fac {comp :: Integer}
            | Mrt {num :: Integer}
            | Llt {expt :: Integer}
            | Fpp {bits :: Integer, num :: Integer}
            deriving (Show, Data, Typeable, Eq)
  
factor = Fac {
  comp = def &= typ "COMPOSITE-INTEGER" &= argPos 0
} &= help "Find prime factors"
  
mrtest = Mrt {
  num = def &= typ "TEST-INTEGER" &= argPos 0
} &= help "Test for primality using ⌈log₄(n)⌉ rounds of Miller-Rabin"

lltest = Llt {
  expt = def &= typ "MERSENNE-EXPONENT" &= argPos 0
} &= help "Test mersenne number for primality using Lucas-Lehmer"

findpp = Fpp {
  bits = def &= typ "BIT-LENGTH" &= argPos 0,
  num = def &= typ "NUM-PRIMES" &= argPos 1
} &= help "Pseudo-random search for NUM-PRIMES probable primes of size BIT-LENGTH using Miller-Rabin"

myModes = cmdArgsMode $ modes [factor,mrtest,lltest,findpp]
  &= help _PROGRAM_USAGE
  &= program _PROGRAM_NAME
  &= summary _PROGRAM_SUMMARY

optionHandler :: Putil -> IO ()
optionHandler opts@Fac{..} = do
  putStrLn . pretty $ pfact comp
optionHandler opts@Mrt{..} = do
  putStrLn . show $ mrt num
optionHandler opts@Llt{..} = do
  putStrLn . show $ llt expt
optionHandler opts@Fpp{..} = do
  putStrLn . show $ searchPrimes bits num

main :: IO ()
main = do
  args <- getArgs
  opts <- (if null args then withArgs ["--help"] else id) $ cmdArgsRun myModes
  optionHandler opts
