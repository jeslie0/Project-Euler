module P001 where

-- * Question
-- If we list all the natural numbers below 10 that are multiples of 3
-- or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
-- Find the sum of all the multiples of 3 or 5 below 1000.
-- * Solution
-- One approach is to generate the list of integers from 0 to 999,
-- then filter out by multiples of 3 or 5, taking the sum at the
-- end. The time complexity will be linear in respect to the length of
-- the list.

approach1 :: Int
approach1 = sum $ filter (\n -> n `mod` 3 == 0 || n `mod` 5 == 0) [0..999]

-- Another approach is to constructively build the list of multiples
-- of 3 and 5, taking care of to deal with multiples of 15; however,
-- this approach is still linear in \(n\). A better approach is to
-- analyse it mathematically. The sum of all the multiples of m under n is
-- given by \(\Sigma_{i=0}^{\floor{\frac n 3}} mi\), which allows us
-- is a standard sum. This gives the following as a solution.

approach2 :: Int
approach2 = 3 * sumto (999 `div` 3)
            + 5 * sumto (999 `div` 5)
            - 15 * sumto (999 `div` 15)
  where
    sumto n = n * (n + 1) `div` 2



main :: IO ()
main = print approach2
