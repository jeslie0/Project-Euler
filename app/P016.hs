module P016 where

foo :: (Integral a, Show a) => a -> Int
foo = sum . map (read . return) . show

p016 = foo (2 ^ 1000)
