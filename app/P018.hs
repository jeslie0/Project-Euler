module P018 where
import Data.List

-- Idea: The triangle list will be of type [[a]]. We can make a Tree of paths through it.

type Triangle = [[Int]]

data Tree a = Node { _info      :: a
                   , _leftTree  :: Tree a
                   , _rightTree :: Tree a
                   }
            | Leaf { _info :: a }
            deriving (Eq)

instance (Show a) => Show (Tree a) where
  show (Leaf a) = "Leaf " ++ show a
  show (Node a left right) = "Node " ++ show a ++ " (" ++ show left ++ ") (" ++ show right ++ ")"

instance Functor Tree where
  fmap f (Leaf a)            = Leaf (f a)
  fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)

-- I want to make a tree of all possible paths through our triangle. Each node will contain an Int which is interpreted as the index of the element in the corresponding list. It will have two subtrees for each of its children


navTree' :: Int -> Tree Int
navTree' n = Node n (navTree' n) (navTree' (n+1))

navTree :: Tree Int
navTree = navTree' 0

cutTree :: Int -> Tree a -> Tree a
cutTree 0 (Leaf a) = Leaf a
cutTree 0 (Node a left right) = Leaf a
cutTree n (Node a left right) = Node a (cutTree (n-1) left) (cutTree (n-1) right)
cutTree n (Leaf a) = Leaf a

test :: Triangle
test = [[3]
       ,[7, 4]
       ,[2, 4, 6]
       ,[8, 5, 9, 3]
       ]
triangle :: Triangle
triangle = [[75]
           ,[95, 64]
           ,[17, 47, 82]
           ,[18, 35, 87, 10]
           ,[20, 04, 82, 47, 65]
           ,[19, 01, 23, 75, 03, 34]
           ,[88, 02, 77, 73, 07, 63, 67]
           ,[99, 65, 04, 28, 06, 16, 70, 92]
           ,[41, 41, 26, 56, 83, 40, 80, 70, 33]
           ,[41, 48, 72, 33, 47, 32, 37, 16, 94, 29]
           ,[53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14]
           ,[70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57]
           ,[91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48]
           ,[63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31]
           ,[04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23]]

-- The point is to use the tree to give directions on how to navigate the triangle.
-- foo :: Tree Int -> Triangle -> [[Int]]
-- foo _ []                       = [[]]
-- foo _ [[a]]                    = [[a]]
-- foo (Leaf n) (x:_)             = [[x !! n]]
-- foo (Node n left right) (x:xs) = [((x !! n) :g), (x !! n):h | g <- foo left xs, h <- foo right xs]

-- p018 :: Int
-- p018 = sum $ maximumBy (\l1 l2 -> compare (sum l1) (sum l2)) $ nub $ foo navTree test

-- main :: IO ()
-- main = print p018

tree :: Tree Int
tree = Node 0 (Node 0 (Leaf 0) (Leaf 1)) (Node 1 (Leaf 1) (Leaf 2))

fff :: [[Int]] -> [Int] -> [Int]
fff [] _ = []
fff _ [] = []
fff (x:xs) (n:ns) = x !! n : fff xs ns

tree2Lists :: Tree a -> [[a]]
tree2Lists (Leaf a) = [[a]]
tree2Lists (Node a left right) = map (a:) (tree2Lists left ++ tree2Lists right)


paths' :: Tree [a] -> [[a]] -> [[a]]
paths' (Leaf a) xs = a:xs
paths' (Node a left right) xs = paths' (fmap (a++) left) (paths' (fmap (a++) right) xs)

paths :: Tree a -> [[a]]
paths tr = paths' (fmap return tr) []

summ :: Tree a -> [[a]]
summ (Leaf a) = [[a]]
summ (Node a l r) = do
  t <- [l, r]
  map (a:) (summ t)


p018' :: [[Int]] -> [[Int]]
p018' xs = map (fff $ xs) (summ (cutTree (length xs -1) navTree))

ans = sum $ maximumBy (\a b -> compare (sum a) (sum b)) (p018' triangle)
