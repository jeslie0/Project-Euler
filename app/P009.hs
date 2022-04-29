module P009 where
import Control.Monad (guard)


-- * Question
-- A Pythagorean triplet is a set of three natural numbers, \(a < b <
-- c\), for which, \(a^2 + b^2 = c^2\)
--
-- For example, \(3^2 + 4^2 = 9 + 16 = 25 = 5^2\).
--
-- There exists exactly one Pythagorean triplet for which \(a + b + c
-- = 1000\). Find the product abc.

-- * Solution
-- We combine the two equations to eliminate the \(c\) parameter.
-- \[2ab - 2000(a+b) = 1000^2\]
-- Now we just need to find the pair (a,b) satisfying this
-- equation. We simplify further:
--
-- \[a = \frac{1000b-1000\times 500}{b-1000}.\]
--
-- Solving further, we see that we can write \(a\) as
--
-- \[a = 1000 + \frac{1000\times 500}{b - 1000}.\]
--
-- For \(a\) to be an integer in the required range, \(b\) must be of
-- the form \(2^i5^j\) for \(0 \leq i, j \leq 3\) but \(i \neq 3 \neq
-- j\). We then just need to check which case makes a into an integer.

-- | This takes in a candidate for \(b\) and outputs the required
-- fraction. We will later check if this an integer.
bFunc :: (Fractional a, Integral b) => b -> a
bFunc n = 1000 * 500 / (fromIntegral n - 1000)

-- | This takes in a value for \(b\) and gives the corresponding value for \(a\)
aFunc :: (Integral a, Integral b) => a -> b
aFunc n = fst . properFraction $ 1000 + bFunc n

-- | This is the list of possibilities for \(a\) and \(b\). We
-- generate a list of possible \(b\) values, then filter out only
-- looking at the values which make bFunc an integer. Then we filter
-- out by which values make a and b be in the correct range. We
-- currently don't worry about forcing \(a\) to be less than \(b\) -
-- that actually doesn't hold here, but by swapping \(a\) and \(b\),
-- it holds.
poss :: [(Int, Int)]
poss = do
  i <- [0..3]
  j <- [0..3]
  guard . not $ i == 3 && j == 3
  let b = 2^i * 5^j
  guard $ 0 == (snd . properFraction . bFunc $ b) -- Is \(bFunc b\) an int?
  guard $ 0 < b && b < 1000
  let a = aFunc b
  guard $ 0 < a && a < 1000
  return (a, b)


p009 :: Int
p009 = let (a, b) = head poss
           c = 1000 - a - b
       in a * b * c

main :: IO ()
main = print p009
