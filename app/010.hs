module Euler010 where
import qualified Data.Map as Map



primes :: [Int]
primes = 2 : [x | x <- [3,5..], isPrime x]

isPrime :: Int -> Bool
isPrime n = all (\p -> n `mod` p > 0) (factorsToTry n)
  where
    factorsToTry x = takeWhile (\p -> p * p <= x) primes


sieve :: (Ord k, Num k) => [k] -> [k]
sieve xs = sieve' xs Map.empty
  where
    sieve' [] table = []
    sieve' (x:xs) table =
      case Map.lookup x table of
        Nothing -> x : sieve' xs (Map.insert (x*x) [x] table)
        Just facts -> sieve' xs (foldl reinsert (Map.delete x table) facts)
          where
            reinsert table prime = Map.insertWith (++) (x+prime) [prime] table

primes' :: [Integer]
primes' = sieve [2..]
