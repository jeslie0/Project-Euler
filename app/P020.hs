module P020 where

import Control.Monad.State.Lazy
  ( MonadState (get, put),
    State,
    evalState,
    runState,
  )

-- * Question

-- \(n!\) means \(n \times (n − 1) \times ... \times 3 \times 2 \times 1\)
--
-- For example, \(10! = 10 \times 9 \times ... \times 3 \times 2
-- \times 1 = 3628800\), and the sum of the digits in the number 10!
-- is \(3 + 6 + 2 + 8 + 8 + 0 + 0 = 27\).
--
-- Find the sum of the digits in the number 100!

-- * Solution

-- To simplify this, we want to figure out what the greatest power of
-- 10 that divides 100! is, then compute the product without this
-- factor, since each power of 10 corresponds to a 0 in the Integer
-- expression. To do this, we will start with the desired list, then
-- count and remove each factor of 5 that occurs. in ea
-- factors of 2 and 5 at an equal rate.

-- | We take a list and count every multiple of 5 into the state, with multiplicity,
-- and then remove all the 5 factors from each number.
helper5 :: [Integer] -> State Integer [Integer]
helper5 [] = return []
helper5 (n : ns) = do
  restOfList <- helper5 ns
  n' <- clean n
  return $ n' : restOfList
  where
    clean n
      | n `mod` 5 == 0 = do
        st <- get
        put $ st + 1
        clean $ n `div` 5
      | otherwise = return n

-- | Takes a list and removes 2 factors until the given state is 0.
helper2 :: [Integer] -> State Integer [Integer]
helper2 [] = return []
helper2 (n : ns) = do
  st <- get
  if st == 0
    then return $ n : ns
    else do
      n' <- clean n
      restOfList <- helper2 ns
      return $ n' : restOfList
  where
    clean n
      | n `mod` 2 /= 0 = return n
      | otherwise = do
        st <- get
        if st /= 0
          then do
            put $ st - 1
            clean $ n `div` 2
          else return n

-- | This is 100! with no trailing 0s.
cutTrail :: Integer
cutTrail =
  product $
    let (cleanList, fives) = runState (helper5 [1 .. 100]) 0
     in evalState (helper2 cleanList) fives

p020 :: Integer
p020 = sum . map ((read :: String -> Integer) . \n -> [n]) . show $ cutTrail

main :: IO ()
main = print p020
