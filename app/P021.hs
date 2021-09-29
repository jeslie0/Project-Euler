module P021 where

d :: Int -> Int
d n = sum properDivisors
  where
    properDivisors = filter (\i -> n `rem` i == 0) [1..n-1]

d2 :: Int -> Int
d2 = d . d

-- The idea is to sum up all of the fixed points of d2.

p21 = sum amicable
  where
    amicable = filter (\i -> d2 i == i && d i /= i) [1..10000-1]
