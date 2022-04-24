module P004 where
import Control.Monad
import Data.List
import qualified Data.Vector as V

-- * Question
-- A palindromic number reads the same both ways. The largest
-- palindrome made from the product of two 2-digit numbers is 9009 =
-- 91 × 99.
-- Find the largest palindrome made from the product of two 3-digit
-- numbers.


-- * Solution
-- A general plan is to multiply all (order increasing) pairs of three
-- digit numbers, then filter out by an "isPalindrome" predicate, then
-- find the maximum.
-- We come up with a function to determine if a list is a palindrome.

-- | \(\mathcal O (n^2)\), where \(n\) is the length of the input
-- list.
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome xs
  | head xs == last xs = isPalindrome . init . tail $ xs
  | otherwise          = False

-- For this problem, \(n\) will be no more than 6, so the upper bound
-- on the number of checks per int will be 36.

-- We need to convert an int to a list of its digits.
-- | \(\mathcal O (\log n)\).
int2list :: Int -> [Int]
int2list m
 | m < 10    = [m]
 | otherwise = m `rem` 10 : int2list (m `div` 10)


-- Now, we need to generate a list of all products of three digit numbers.
-- | A list of all products of three digit numbers. This has length
-- \(\frac{n(n+1)}{2}\), where \(n\) is the length of the list we
-- extract a from.
prodList :: [Int]
prodList = [ a * b | a <- [100..999], b <- [a..999] ]

-- The solution is then given by finding the maximum palindrome in the list.

p004 :: Int
p004 = maximum $ filter (isPalindrome . int2list) prodList

main :: IO ()
main = print p004

-- * Improvements
-- We can improve the efficiency of isPalindrome by converting the
-- list to a vector first, then checking if the vector is a
-- palindrome. This is because we spend a lot of effort extracting
-- initial segments of sublists and the last element of the list. This
-- is done in linear time for linked lists, while it is done in
-- constant time for vectors.


-- | \(\mathcal O (n)\).
isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' = isVecPalindrome . V.fromList
  where
    isVecPalindrome :: (Eq a) => V.Vector a -> Bool
    isVecPalindrome vec
      | V.length vec == 0 = True
      | V.length vec == 1 = True
      | otherwise         =
        (V.head vec == V.last vec) && (isVecPalindrome . V.init . V.tail $ vec)

-- Using this, we can speed up solution, but not by much. This is
-- because we are only checking if a list of size at most 6 is a
-- palindrome. For such small values, this has a minimal impact on the
-- run time. It would be faster to come up with a constructive way of
-- making =prodList=, rather than filtering a list out.

p004' :: Int
p004' = maximum $ filter (isPalindrome' . int2list) prodList
