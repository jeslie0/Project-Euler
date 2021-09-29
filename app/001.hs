problem1 :: Int
problem1 = foldl (\b a -> b + a) 0 $ filter (\n -> (n `mod` 3 == 0 || n `mod` 5 == 0)) [1..999]
