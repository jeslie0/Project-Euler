module P015 where

data Directions
  = Right
  | Down
  deriving (Eq, Show)

data Tree a
  = Leaf
  | Node a (Tree a) (Tree a)
  deriving (Eq, Show)


factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)
