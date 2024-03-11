module P023 where

import Data.Foldable
import Data.Set qualified as Set
import Debug.Trace (trace)
import ProjectEuler.Primes (listOfFactors)

-- * Question

-- A perfect number is a number for which the sum of its proper
-- divisors is exactly equal to the number. For example, the sum of the
-- proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means
-- that is a perfect number.
--
-- A number n is called deficient if the sum of its proper divisors is
-- less than and it is called abundant if this sum exceeds n.
--
-- As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the
-- smallest number that can be written as the sum of two abundant
-- numbers is 24. By mathematical analysis, it can be shown that all
-- integers greater than 28123 can be written as the sum of two
-- abundant numbers. However, this upper limit cannot be reduced any
-- further by analysis even though it is known that the greatest number
-- that cannot be expressed as the sum of two abundant numbers is less
-- than this limit.
--
-- Find the sum of all the positive integers which cannot be written
-- as the sum of two abundant numbers.

-- * Answer
-- We construct a set of all sums of two abundant numbers (under the
-- given limit), then sum up the elements in the set and subtract it
-- from the sum of all the numbers up to the limit.

isAbundant :: Int -> Bool
isAbundant n
  | n <= 1 = False
  | otherwise = sum (listOfFactors n) > n

abundantNumbers :: [Int]
abundantNumbers = [n | n <- [12 ..], isAbundant n]
{-# NOINLINE abundantNumbers #-}


abundantSumsLessThan :: Int -> Set.Set Int
abundantSumsLessThan limit = Set.fromList [ x + y | x <- abundantsInRange 12 limit, y <- abundantsInRange 12 (limit `div` 2), x + y <= limit]
  where
    abundantsInRange n m = dropWhile (< n) . takeWhile (<= m) $ abundantNumbers

nonAbundantSums :: Int -> Int
nonAbundantSums limit =
  limit * (limit + 1) `div` 2 - sum (abundantSumsLessThan limit)

p023 :: Int
p023 = nonAbundantSums 28123
