module P023 where

d :: Int -> Int
d n = sum properDivisors
  where
    properDivisors = filter (\i -> n `rem` i == 0) [1..n-1]

abundant :: Int -> Bool
abundant n = d n > n


abundantLessThan = filter abundant [1..28123-12]
