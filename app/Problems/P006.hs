module P006 where

-- * Question
-- The sum of the squares of the first ten natural numbers is,
-- \[1^2 + 2^2 + \ldots + 10^2 = 385\]
-- The square of the sum of the first ten natural numbers is,
-- \[(1 + 2 + \ldots + 10)^ = 55^2 = 3025\]
-- Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is
-- \[3025 - 385 = 2640.\]
-- Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

-- * Solution
-- The sum of square from 1 to \(n\) is given by the formula
-- \(\frac{n(n+1)(2n+1)}{6}\). The sum from 1 to \(n\), squared is
-- given by \(\left(\frac 1 2 n(n+1)\right)^2\).

-- | This formula overflows the Int size, so we need it to be an Integer
p006' :: Integer
p006' = let n = 100
       in (n * (n + 1) `div` 2) ^ 2
          - (n * (n + 1) * (2* n + 1) `div` 6)

-- | We can simplify the above expression to get something that
-- doesn't overflow, and has the speed benefit of every term begin an
-- Int.
p006 :: Int
p006 = let n = 100
        in ((n ^ 4) `div` 4)
           + ((n ^ 3) `div` 6)
           - ((n ^ 2) `div` 4)
           - (n `div` 6)

main :: IO ()
main = print p006
