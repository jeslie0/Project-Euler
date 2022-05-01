module P015 where

-- * Question
-- Starting in the top left corner of a 2×2 grid, and only being able
-- to move to the right and down, there are exactly 6 routes to the
-- bottom right corner.
--
-- how many such routes are there through a 20x20 grid?

-- * Solution
-- For an \(m \times n\) grid, the number of paths from the top left to
-- the bottom right corner is given by the recurrence relation
-- \[ r_{m,n} = r_{m-1,n} + r_{m, n-1}, r_{k,1} = r_{1,k} = 1. \]
-- We could try and solve this, however this a more standard
-- combinatorial approach. We can think of a path as being a \(m+n\)
-- list of directions, with exactly \(m\) downs and \(n\) rights. This
-- problem then becomes figuring out how many ways we can choose \(m\)
-- from \(n + m\).

fac :: (Integral a) => a -> a
fac 0 = 1
fac n = product [1..n]

p015 :: Integer
p015 = fac 40 `div` (fac 20 * fac 20)

main :: IO ()
main = print p015
