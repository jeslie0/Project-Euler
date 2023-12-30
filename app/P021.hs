module P021 where

d :: Int -> Int
d n =
  let lowFactors = filter (\i -> n `mod` i == 0) $ takeWhile (\i -> i ^ 2 <= n) [1 ..]
   in sum $ foldl (\lst i -> if i ^ 2 < n then (n `div` i) : lst else lst) lowFactors (tail lowFactors)

isAmicable :: Int -> Bool
isAmicable n =
  let b = d n
   in (b /= n) && d b == n

p021 :: IO ()
p021 = print . sum $ filter isAmicable [1 .. 100000 - 1]


main = print "hi"
