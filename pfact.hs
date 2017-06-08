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
import System.Console.CmdArgs

--import Debug.Trace
--debug = (flip trace) False

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

main :: IO ()
main = do
  Options{..} <- cmdArgs options
  putStrLn . pretty . pfact num $ seed

pcycle :: Integral a => a -> a -> a -> a
pcycle x a N = (x^2 + a) `mod` N



pRho :: Integral a => a -> a -> a -> a -> Maybe a
pRho n x y c
--  | debug ("(pRho " ++ show n ++ " " ++ show x ++ " " ++ show y ++
--           ") -> a = " ++ show a ++ " b = " ++ show b) = undefined
  | a == b    = Nothing
  | p > 1     = Just p
  | otherwise = pRho n a b c  -- p == 1
  where a = pcycle x c n
        b = pcycle (pcycle y c n) c n
        p = gcd (abs (b - a)) n

pollardsRhoPermute :: Integral a => a -> a -> a -> a
pollardsRhoPermute n s t
--  | debug ("(pollardsRhoPermute " ++ show n ++ " " ++ show s ++ " " ++ show t ++
--           ") -> c = " ++ show c ++ " p = " ++ show p) = undefined
  | t == 5 = n
  | p == Nothing = pollardsRhoPermute n c (t + 1) -- Pollard's Rho failed so permute the constant and try again
  | otherwise = fromJust p
  where c = lcg_lehmer s
        p = pollardsRho n s s c

pfact :: (Show s, Integral s) => s -> s -> [s]
pfact n s = sort . factorize n $ s

factorize :: Integral a => a -> a -> [a]
factorize n s
--  | debug ("*** factorize n = " ++ show n) = undefined
  | even n = [2] ++ factorize (quot n 2) s
  | n == 1 = [ ]
  | n == z = [n]
  | otherwise = factorize z s ++ factorize (quot n z) s
  where z = pollardsRhoPermute n s 1

pretty :: (Show a, Eq a) => [a] -> String
pretty x = prettyR x 1

prettyR :: (Show a1, Show a, Ord a, Num a, Eq a1) => [a1] -> a -> String
prettyR [x] 1 = show x
prettyR [x] c = show x ++ "^" ++ show c
prettyR (x:y:xs) c
  | x /= y && c > 1 = show x ++ "^" ++ show c ++ " x " ++ prettyR (y:xs) 1
  | x /= y = show x ++ " x " ++ prettyR (y:xs) 1
  | otherwise = prettyR (y:xs) (c+1)
