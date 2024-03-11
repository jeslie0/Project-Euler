module P010 where

import Primes
-- * Question
-- The sum of the primes below 10 is \(2 + 3 + 5 + 7 = 17\).
--
-- Find the sum of all the primes below two million.

-- * Solution
-- We just sum up the required primes from out list of primes.

p010 :: Int
p010 = sum . takeWhile (<= 2000000) $ primes

main = print p010
