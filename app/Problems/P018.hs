module P018 where

import qualified Data.Vector as V

-- * Question
-- By starting at the top of the triangle below and moving to adjacent
-- numbers on the row below, the maximum total from top to bottom is
-- 23.
--
-- 3
-- 7 4
-- 2 4 6
-- 8 5 9 3
--
-- That is, 3 + 7 + 4 + 9 = 23.
--
-- Find the maximum total from top to bottom of the triangle below:
--
-- 75
-- 95 64
-- 17 47 82
-- 18 35 87 10
-- 20 04 82 47 65
-- 19 01 23 75 03 34
-- 88 02 77 73 07 63 67
-- 99 65 04 28 06 16 70 92
-- 41 41 26 56 83 40 80 70 33
-- 41 48 72 33 47 32 37 16 94 29
-- 53 71 44 65 25 43 91 52 97 51 14
-- 70 11 33 28 77 73 17 78 39 68 17 57
-- 91 71 52 38 17 14 91 43 58 50 27 29 48
-- 63 66 04 68 89 53 67 30 73 16 69 87 40 31
-- 04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
--
-- NOTE: As there are only 16384 routes, it is possible to solve this
-- problem by trying every route. However, Problem 67, is the same
-- challenge with a triangle containing one-hundred rows; it cannot be
-- solved by brute force, and requires a clever method! ;o)

-- * Answer
-- A nice solution would be to use a greedy algorithm, where at each
-- level we choose the maximum child. This won't necessarily give us
-- the desired result though. An alternative is to do this approach,
-- but starting from the base of the triangle. This will give us the
-- maximal result, but we won't know what the path is. Since we just
-- care about the result, that is okay.
--
-- The idea is that we start from the second last row and replace each
-- element with the sum of itself and its largest child. We then kill
-- the last row, leaving the augmented row as the new final row. Then,
-- we recurse until we have one row left, which contains one element -
-- the largest path. A simple proof by induction shows that this
-- algorithm generates the maximum sum, mainly by realising that each
-- step doesn't change the maximal path, but only truncates it.

maximalPathSum :: (Num a, Ord a) => V.Vector (V.Vector a) -> a
maximalPathSum xs = maximalPathSum' xs V.! 0 V.! 0
  where
    maximalPathSum' :: (Num a, Ord a) => V.Vector (V.Vector a) -> V.Vector (V.Vector a)
    maximalPathSum' xs
      | V.length xs == 0 = xs
      | V.length xs == 1 = xs
      | otherwise      =
          let index = V.length xs - 2 -- | position of row to change
              newRow = V.zipWith (\n pair -> n + maximum [fst pair, snd pair]) (xs V.! index) (makePairs index . V.last $ xs)
              newGrid = V.take index xs <> V.singleton newRow
          in maximalPathSum' newGrid
    makePairs index vec = V.generate (index + 1) (\i -> (vec V.! i, vec V.! (i+1)))

triangle :: [[Int]]
triangle =
  [[75],
   [95, 64],
   [17, 47, 82],
   [18, 35, 87, 10],
   [20, 04, 82, 47, 65],
   [19, 01, 23, 75, 03, 34],
   [88, 02, 77, 73, 07, 63, 67],
   [99, 65, 04, 28, 06, 16, 70, 92],
   [41, 41, 26, 56, 83, 40, 80, 70, 33],
   [41, 48, 72, 33, 47, 32, 37, 16, 94, 29],
   [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14],
   [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57],
   [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48],
   [63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31],
   [04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23]]


p018 :: Int
p018 = maximalPathSum . V.fromList . map V.fromList $ triangle

main :: IO ()
main = print p018
