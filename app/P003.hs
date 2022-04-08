module P003 where

import Data.List
import Primes

-- * Question
-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600851475143?

-- * Solution
-- We use the efficient prime generation from the Primes library to
-- construct a list of all primes. Then, we extract the largest prime
-- factor from the list of factors.

solution :: Int
solution = maximum . primeFactors $ 600851475143

main = print solution
