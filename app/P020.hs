module P020 where

import Control.Monad.State

initList :: [Integer]
initList = [1 .. 100]

greatFilter :: (Integral a) => [a] -> State (Int, Int) [a]
greatFilter [] = return []
greatFilter (n:ns) = do
  (m2, m5) <- get
  if m2 /= 0 && even n
    then do
      put (m2 - 1, m5)
      greatFilter (n `div` 2 : ns)
    else if m5 /= 0 && n `rem` 5 == 0
           then do
             put (m2, m5 - 1)
             greatFilter (n `div` 5 : ns)
           else do
             ints <- greatFilter ns
             return $ n : ints

p020 :: Integer
p020 = sum $ map (\a -> read [a]) (show num)
  where
    num = product $ evalState (greatFilter initList) (24, 24)

test :: Int -> Int
test n =
  let foo = 8
      foobar89 = 8
   in foo

foo n = test n
