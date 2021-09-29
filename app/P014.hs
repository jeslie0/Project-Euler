module P014 where

import           Control.Monad.State
import           Data.Foldable
import           Data.List
import qualified Data.Map            as Map

collatz :: Int -> Int
collatz n
  | even n    = n `div` 2
  | otherwise = 3 * n + 1


collatzSequence :: Int -> [Int]
collatzSequence 1 = [1]
collatzSequence n = n : collatzSequence (collatz n)

collatzLength :: Int -> Int
collatzLength = length . collatzSequence

-- p14 :: Maybe Int
p14 =
  let list = map collatzLength [1..1000000]
      max  = maximum list
  in elemIndex max list


-- Idea: use a dictionary to build up a list of collatz sequences. If we need to figure out the rest of a collatz sequence, we look in the dictionary to see if the number is in there. If it is, we just add add it on. This seems like a state! Make the state be the dictionary and we update the state...

-- collatz' :: [Int] -> Map.Map Int [Int] -> Map.Map Int [Int]
-- collatz' [] _     = Map.empty
-- collatz' (n:ns) m = case n `Map.lookup` m of
--                       Just ns ->


-- f :: [Int] -> State (Map.Map Int [Int]) [[Int]]
-- f [] = state $ \s -> ([], s)
-- f (n:ns) = do
--   m <- get
--   case n `Map.lookup` m of
--     Just xs -> return (xs:f ns)
--     Nothing -> return (evalState (g n) m:f ns)

-- g :: Int -> State (Map.Map Int [Int]) [Int]
-- g n = do
--   m <- get
--   put $ Map.insert n [n] m
--   let n' = collatz n
--   m' <- get
--   case n' `Map.lookup` m' of
--     Just xs -> do
--       put $ Map.insertWith (++) n xs m'
--       return (n:xs)
--     Nothing ->
--       g n'

-- New plan: Just make and update a dictionary!!! When we are finished we can extract the list from that. An issue may happen with lazy evaluation, but this needs to be tested.

-- func :: [Int] -> Map.Map Int [Int] -> Map.Map Int [Int]
-- func [] m     = m
-- func (n:ns) m =
--   case n `Map.lookup` m of
--     Just xs -> func ns m
--     Nothing ->
--       let m' = Map.insert n [n] m
--           n' = collatz n
--       in gunc n m

gunc :: Int -> State (Map.Map Int [Int]) [Int]
gunc 1 = state $ \m -> ([1], Map.insert 1 [1] m)
gunc n = do
  m <- get
  case n `Map.lookup` m of
    Just xs -> return xs
    Nothing -> do
      foo <- gunc (collatz n)
      let collatzSeq = n : foo
      m' <- get
      put $ Map.insert n collatzSeq m'
      return collatzSeq

func :: [Int] -> State (Map.Map Int [Int]) [[Int]]
func [] = state $ \m -> ([], m)
func (n:ns) = do
  m <- get
  case n `Map.lookup` m of
    Just xs -> do
      foo <- func ns
      return (xs:foo)
    Nothing -> do
      collatzSeq <- gunc n
      m' <- get
      put $ Map.insert n collatzSeq m'
      foo <- func ns
      return $ collatzSeq : foo


po14' :: Int
po14' =
  head $
  maximumBy (\x y -> compare (length x) (length y)) $
  evalState (func [1 .. 1000000 - 1]) Map.empty
