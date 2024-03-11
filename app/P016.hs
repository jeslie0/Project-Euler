module P016 where

-- * Question
-- \(2^{15} = 32768\) and the sum of its digits is \(3 + 2 + 7 + 6 + 8
-- = 26\).
-- What is the sum of the digits of the number \(2^{1000}\)?

-- * Solution
-- A brute force solution is to simply evaluate \(2^{1000}\) as an
-- integer, then read it to a list of digits and then sum.
p016' :: Int
p016' = sum (map (read . pure) (show $ 2 ^ 1000))

main :: IO ()
main = print p016'
