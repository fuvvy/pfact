
{------------------------------------------------------------------------------
--
-- A small application for testing primality and finding prime factorizations
-- This code evolved from a solution for Project Euler's third problem.
--
-------------------------------------------------------------------------------}
{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main where

--import System.Console.CmdArgs
--import System.Environment (getArgs, withArgs)
--import System.Exit
import PFact
import Primality
{-
_PROGRAM_NAME     = "putil"
_PROGRAM_VERSION  = "1.0"
_PROGRAM_INFO     = _PROGRAM_NAME ++ " v" ++ _PROGRAM_VERSION ++ ", " ++ _AUTHOR
_PROGRAM_DESC     = "Primality testing and prime factorization utility"
_PROGRAM_SUMMARY  =  _PROGRAM_INFO ++ "\n" ++ _PROGRAM_DESC
_PROGRAM_USAGE    = "Examples:\n\tputil fac 3895462145894328\n\tputil mrt 18848997157\n\tputil llt 110503"
_AUTHOR           = "(A) github.com/fuvvy"

data Putil  = Fac {comp :: Integer, seed :: Integer}
            | Mrt {num :: Integer, seed :: Integer}
            | Llt {expt :: Integer, seed :: Integer}  
            deriving (Show, Data, Typeable, Eq)

seedFlag x = x &= help "Provide your own seed" &= typ "INTEGER+"
  
factor = Fac {
  comp = def &= typ "COMPOSITE+" &= argPos 0,
  seed = seedFlag 2147483647
} &= help "Find prime factors"
  
mrtest = Mrt {
  num = def &= typ "INTEGER+" &= argPos 0,
  seed = seedFlag 32416190071
} &= help "Test for primality using ⌈log₄(n)⌉ rounds of Miller-Rabin"

lltest = Llt {
  expt = def &= typ "EXPONENT+" &= argPos 0,
  seed = seedFlag 32416189381
} &= help "Test mersenne number for primality using Lucas-Lehmer"

myModes = cmdArgsMode $ modes [factor,mrtest,lltest]
  &= help _PROGRAM_USAGE
  &= program _PROGRAM_NAME
  &= summary _PROGRAM_SUMMARY
      
optionHandler :: Putil -> IO ()
optionHandler opts@Fac{..}  = do
  putStrLn . pretty . pfact comp $ seed
optionHandler opts@Mrt{..}  = do
  putStrLn . show . mrt num $ seed
optionHandler opts@Llt{..}  = do
  putStrLn . show . llt expt $ seed
 -}

main :: IO ()
main = do
  putStrLn . show . llt 110503 $ 825250931
  --args <- getArgs
  --opts <- (if null args then withArgs ["--help"] else id) $ cmdArgsRun myModes
  --optionHandler opts
