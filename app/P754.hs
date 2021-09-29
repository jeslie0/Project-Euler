module P754 where

import           Control.Monad.State
import qualified Data.Map            as Map
import Data.Foldable

gcd' :: Integral a => a -> a -> a
gcd' n m
  | n `rem` m == 0 = m
  | otherwise      = gcd' m (n `rem` m)

coprime :: Integral a => a -> a -> Bool
coprime n m
  | gcd' n m == 1 = True
  | otherwise     = False


g :: Integral a => a -> a
g n = product $ filter (\i -> n `coprime` i) [1..n]

bigG :: (Integral a) => a -> a
bigG 1 = g 1
bigG n = g n * bigG (n-1)



-- Idea: Build up a dictionary of gcds, encoded as a state

updateStateMap :: Ord a => a -> b -> State (Map.Map a b) ()
updateStateMap a b = do
  curState <- get
  let updatedMap = Map.insert a b curState
  put updatedMap



statefulGCD :: Integral a => a -> a -> State (Map.Map (a, a) a) a
statefulGCD n m = do
  curState <- get
  case Map.lookup (n,m) curState of
    Just x  -> return x
    Nothing -> do
      if n `rem` m == 0
        then do
        updateStateMap (n, m) m
        return m
        else do
         x <- statefulGCD m (n `rem` m)
         updateStateMap (m, n `rem` m) x
         return x


coprime' :: Integral a => a -> a -> State (Map.Map (a,a) a) Bool
coprime' n m = do
  re <- statefulGCD n m
  if re == 1
    then return True
    else return False

g' :: Integral a => a -> State (Map.Map (a,a) a) a
g' n = do
  lst <- filterM (\i -> n `coprime'` i) [1..n]
  return $ (product lst) `rem` 1000000007

-- bigG' :: (Integral a) => a -> State (Map.Map (a,a) a) a
-- bigG' 1 = g' 1
-- bigG' n = do
--   recur <- bigG' (n-1)
--   top <- g' n
--   return (top * recur)


executeG :: Integral a => a -> a
executeG n = evalState (bigG' n) Map.empty


remMultiply :: (Integral a) => [a] -> a -> a
remMultiply xs n = foldr' (\a b -> a * (b `rem` n)) 1 xs

bigG' :: (Integral a) => a -> State (Map.Map (a,a) a) a
bigG' n = do
  lst <- mapM g' [1..n]
  return $ remMultiply lst 1000000007


executeG' :: Integral a => a -> a
executeG' n = evalState (bigG' n) Map.empty

listOfGs :: (Integral a) => a -> State (Map.Map (a,a) a) [a]
listOfGs n = mapM g' [1..n]
