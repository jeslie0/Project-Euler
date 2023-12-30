module Fibonacci
  ( fibList,
    fib
  ) where

fibList :: (Integral a) => [a]
fibList = 1 : 1 : zipWith (+) fibList (tail fibList)

fib :: (Integral a) => Int -> a
fib n = fibList !! n
