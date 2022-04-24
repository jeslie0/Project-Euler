module P005 where

import Control.Monad.State.Lazy
import Data.List
import qualified Data.Map as M
import Primes

-- * Question

-- 2520 is the smallest number that can be divided by each of the
-- numbers from 1 to 10 without any remainder.
-- What is the smallest positive number that is evenly divisible by
-- all of the numbers from 1 to 20?

-- * Solution

-- The solution to this problem will be given by the product of all
-- the primes less than 20, with each prime power being the largest
-- prime power in a number less than or equal to 20. To this end, we
-- shall construct a function that generates the list of prime powers
-- of a number, then checks to see if they are in a stateful
-- dictionary, updating the dictionary if need be.

p005 :: Int
p005 = primePowers2Int . M.toList $ execState (p005' [1 .. 20]) M.empty
  where
    -- | Builds a stateful dictionary of prime factors with the
    -- highest powers from the integers in the list.
    p005' :: [Int] -> State (M.Map Int Int) ()
    p005' [] = return ()
    p005' (n : ns) = do
      let primefacs = primeFactorPowers n
      updateDict primefacs
      p005' ns
      where
        updateDict :: [(Int, Int)] -> State (M.Map Int Int) ()
        updateDict [] = return ()
        updateDict ((n, i) : ns) = do
          dict <- get
          case M.lookup n dict of
            Nothing -> do
              put $ M.insert n i dict
              updateDict ns
            Just k -> do
              if i >= k
                then do
                  put $ M.insert n i dict
                  updateDict ns
                else updateDict ns

main :: IO ()
main = print p005
