module Primes where

import qualified Data.Map as Map
import Control.Concurrent (writeChan)
import Control.Monad.State.Lazy


-- * Primes
-- This is mostly taken from the Data.Numbers.Primes haskell library
-- and Melissa O'Neil's Genuine Sieve of Eratosthenes.

sieve3 :: (Ord k, Num k) => [k] -> [k]
sieve3 xs = sieve' xs Map.empty
  where
    sieve' [] table = []
    sieve' (x:xs) table =
      case Map.lookup x table of
        Nothing -> x : sieve' xs (Map.insert (x*x) [x] table)
        Just facts -> sieve' xs (foldl reinsert (Map.delete x table) facts)
          where
            reinsert table prime = Map.insertWith (++) (x+prime) [prime] table


wheel2357 :: Integral a => [a]
wheel2357 = 2:4:2:4:6:2:6:4:2:4:6:6:2:6:4:2:6:4:6:8:4:2:4:2:4:8:6:4:6:2:4:6:2:6:6:4:2:4:6:2:6:4:2:4:2:10:2:10:wheel2357

spin :: Num t => [t] -> t -> [t]
spin (x:xs) n = n : spin xs (n + x)

primes :: Integral a => [a]
primes = 2:3:5:7: sieve3 (spin wheel2357 11)

-- | Generates a list of prime factors of n, with repetition.
primeFactors :: Integral a => a -> [a]
primeFactors n = divtest n primes
  where
    divtest 1 _ = []
    divtest _ [] = []
    divtest n (x:xs)
      | n `mod` x == 0 = x : divtest (n `div` x) (x:xs)
      | otherwise      = divtest n xs


primeFactorPowers :: Integral a => a -> [(a, Int)]
primeFactorPowers = collect . primeFactors
  where
    collect :: (Eq a) => [a] -> [(a,Int)]
    collect [] = []
    collect xs =
      let (ns, remaining) = span (\a -> a == head xs) xs
      in (head ns, length ns) : collect remaining

primePowers2Int :: Integral a => [(a, Int)] -> a
primePowers2Int [] = 1
primePowers2Int ((n,i):ns) = n ^ i * primePowers2Int ns
