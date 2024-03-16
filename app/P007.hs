module P007 where
import ProjectEuler.Primes

-- * Question
-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
-- What is the 10001st prime number?

-- * Solution
-- We have already constructed an efficient list of primes, in order,
-- so we use that

p007 :: Int
p007 = primes !! 10000

main = print p007
