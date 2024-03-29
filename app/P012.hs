module P012 where

import ProjectEuler.Primes
import Data.Foldable (find)

-- * Question
-- The sequence of triangle numbers is generated by adding the natural numbers. So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. The first ten terms would be:
--
-- 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
--
-- Let us list the factors of the first seven triangle numbers:
--
--      1: 1
--      3: 1,3
--      6: 1,2,3,6
--     10: 1,2,5,10
--     15: 1,3,5,15
--     21: 1,3,7,21
--     28: 1,2,4,7,14,28
--
-- We can see that 28 is the first triangle number to have over five divisors.
--
-- What is the value of the first triangle number to have over five hundred divisors?

-- * Solution
-- Triangle numbers are generated by \n -> n(n+1)/2. We can compute
-- the number of divisors more efficiently by realising that \(n\) and
-- \(n+1\) share no divisors. We can do less work by computing their
-- factors (taking into account parity) and multiplying the results

-- | Counts the number of factors of the nth triangle number.
numberOfTriangleFactors :: Int -> Int
numberOfTriangleFactors n
  | even n    = numberOfFactors (n `div` 2) * numberOfFactors (n + 1)
  | otherwise = numberOfFactors n * numberOfFactors ((n+1) `div` 2)

p012 :: Maybe Int
p012 = do
  n <- find (\n -> numberOfTriangleFactors n > 500) [1 .. ]
  return $ n * (n + 1) `div` 2

main :: IO ()
main = print p012
