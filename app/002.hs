import           Data.Foldable
import           Data.List

fibTable :: (Integral a) => [a]
fibTable = 0 : 1 : zipWith (+) fibTable (tail fibTable) -- list of fibonacci numbers

biggerThan4Mill :: (Integral a) => [a] -> Maybe a
biggerThan4Mill = find (\n -> n >= 4000000)

take' :: (Integral a) => a -> [b] -> [b]
take' 0 _      = []
take' n (x:xs) = x : take' (n-1) xs

problem2 :: (Integral a) => Maybe a
problem2 = do
  maxVal <- biggerThan4Mill fibTable                     -- Find first term bigger than 4Mill
  maxValPos <- elemIndex maxVal fibTable                 -- Get position of above term
  let smallFibList = take' maxValPos fibTable        -- Takes the sublist up to this term
  let smallEvenFibList = filter (\n -> n `rem` 2 == 0) smallFibList -- removes odd numbers
  let sum = foldl (+) 0 smallEvenFibList                    -- Adds up list
  return sum

problem2' :: (Integral a) => Maybe a
problem2' = biggerThan4Mill fibTable >>= \el -> elemIndex el fibTable >>= \n -> Just (take n fibTable) >>= \xs -> Just (filter (\n -> n `rem` 2 == 0) xs) >>= \xs -> Just (foldl (+) 0 xs)


