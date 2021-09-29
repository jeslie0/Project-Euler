module Euler.Helper where

import           Control.Monad.State
import           Data.List
import qualified Data.Map            as Map

divisors :: Int -> [Int]
divisors n = n:filter (\x -> n `mod` x == 0) [1..(n+1) `div` 2]

numDivisors :: Int -> Int
numDivisors = length . divisors

triangle :: Int -> Int
triangle n = n * (n + 1) `div` 2

p12 :: Maybe Int
p12 = find (> 500) $ map (numDivisors . triangle) [1..]

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


primeFactorisation :: Int -> [(Int,Int)]
primeFactorisation n =
  let smallPrimes = takeWhile (<= n) primes
   in if last smallPrimes == n
        then [(n, 1)]
        else Map.toList (func smallPrimes n)
  where
    func :: [Int] -> Int -> Map.Map Int Int
    func [] m     = undefined
    func (p:ps) m
      | m `mod` p == 0 = undefined


foo :: [Int] -> Int -> State (Map.Map Int Int) [Int]
foo [] _ = state $ \m -> ([] , m)
foo (p:ps) n =
  state $ \m ->
    if n `mod` p == 0
      then (p : ps, Map.insertWith (+) p 1 m)
      else (ps, m)

-- foo' :: [Int] -> Int -> [(Int, Int)]
-- foo' ps n = Map.assocs $ flip execState (Map.empty) $ do


-- ffff :: [Int] -> Int -> Map.Map Int Int -> Map.Map Int Int
-- ffff [] _ m = m
-- ffff (p:ps) n m
--   | p == n         = Map.insertWith (+) p 1 m
--   | n `mod` p == 0 = Map.insertWith (+) p 1 $ ffff (p:ps) (n `div` p) m
--   | otherwise      = ffff ps n m


-- fudge :: [Int] -> Int -> [(Int, Int)]
-- fudge xs n = Map.assocs $ ffff xs n Map.empty
fac 0 = 1
fac n = n * fac (n-1)

rebuildFromFacts :: (Integral b, Num a) => [(a, b)] -> a
rebuildFromFacts = foldr (\(p, m) n -> p^m * n) 1

factorisation :: Integral a => a -> [(a, a)]
factorisation n = Map.assocs $ mapOfFacts (takeWhile (<= n) primes) n Map.empty
  where
    mapOfFacts [] _ m = m
    mapOfFacts (p:ps) n m
      | p == n         = Map.insertWith (+) p 1 m
      | n `mod` p == 0 = Map.insertWith (+) p 1 $ mapOfFacts (p:ps) (n `div` p) m
      | otherwise      = mapOfFacts ps n m

totalFactors :: Integral a => a -> a
totalFactors n =
  let primeFacts = factorisation n
   in product $ map ((+1) . snd) primeFacts
