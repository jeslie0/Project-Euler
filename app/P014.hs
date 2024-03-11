module P014 where

import Control.Monad.State.Lazy
import Control.Monad
import qualified Data.Map as M

-- * Question
-- The following iterative sequence is defined for the set of positive
-- integers:
--
-- n → n/2 (n is even)
-- n → 3n + 1 (n is odd)
--
-- Using the rule above and starting with 13, we generate the
-- following sequence:
-- 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
--
-- It can be seen that this sequence (starting at 13 and finishing at
-- 1) contains 10 terms. Although it has not been proved yet (Collatz
-- Problem), it is thought that all starting numbers finish at 1.
--
-- Which starting number, under one million, produces the longest
-- chain?
--
-- NOTE: Once the chain starts the terms are allowed to go above one
-- million.

-- * Solution
-- We build a dictionary given a mapping between ints and their
-- collatz length. Then our collatz function will check the dictionary
-- before computing the length.

-- | Stateful collatz function. Returns the collatz list and builds a
-- dictionary of all intermediate collatz lists.
collatz :: Int -> State (M.Map Int [Int]) [Int]
collatz 1 = do
  dict <- get
  put $ M.insert 1 [1] dict
  return [1]
collatz n = do
  dict <- get
  case dict M.!? n of
    Just xs -> return xs
    Nothing -> do
      ns <- collatz $ if even n then n `div` 2 else 3 * n + 1
      newDict <- get
      put $ M.insert n (n:ns) newDict
      return $ n:ns

collatzLength :: Int -> State (M.Map Int Int) Int
collatzLength 1 = do
  dict <- get
  put $ M.insert 1 1 dict
  return 1
collatzLength n = do
  dict <- get
  case dict M.!? n of
    Just m -> return m
    Nothing -> do
      m <- collatzLength $ if even n then n `div` 2 else 3 * n + 1
      newDict <- get
      put $ M.insert n (m + 1) newDict
      return (m + 1)


-- | We want to find which number from 1 to 1000000 has the greatest
-- collatz length, so we adapt the function above to also keep track
-- of the maximum collatz sequence found so far. We then monadically
-- map this over the list [1..1000000] and return that which gives the
-- largest collatz sequence. By keeping a rolling update of the
-- maximum, we don't need to traverse the list again searching for it.
p014 :: Int
p014 = fst . fst . execState (mapM collatzMaxLength [1..1000000]) $ ((1, 1), M.empty)
  where
    collatzMaxLength :: Int -> State ((Int, Int), M.Map Int Int) Int
    collatzMaxLength 1 = do
      ((index,potMax), dict) <- get
      put (if 1 >= potMax then (1,1) else (index,potMax), M.insert 1 1 dict)
      return 1
    collatzMaxLength n = do
      (_, dict) <- get
      case dict M.!? n of
        Just m -> return m
        Nothing -> do
          m <- collatzMaxLength $ if even n then n `div` 2 else 3 * n + 1
          ((index, potMax), newDict) <- get
          put (if m+1 >= potMax then (n, m+1) else (index, potMax), M.insert n (m + 1) newDict)
          return (m + 1)

main :: IO ()
main = print p014
