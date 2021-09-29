import Data.List
-- Largest prime factor

primeList :: (Integral a) => [a]
primeList = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `rem` p > 0 && x >= ceiling (sqrt (fromIntegral p)) ]


isPrime :: (Integral a) => a -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime n
  | length [x | x <- [2.. ceiling (sqrt (fromIntegral n))], n `mod` x == 0] >= 1 = False
  | otherwise = True

factors :: (Integral a) => a -> [a]
factors 0 = []
factors 1 = []
factors n = filter (\x -> n `mod` x == 0) [1..n]

primeFactors :: (Integral a) => a -> [a]
primeFactors n
  | isPrime n = [n]
  | otherwise = filter isPrime $ factors n

primeFactors' :: (Integral a) => a -> [a]

primeFactors' n = func $ reverse (tail (factors n))
  where
    func [] = []
    func (x:xs) = if foldl (||) False (map (\n -> x `mod` n /= 0) xs) == False then x : func xs else func xs

largestPrimeFactor :: (Integral a) => a -> a
largestPrimeFactor n = head . reverse $ takeWhile (\x -> x <= ceiling (fromIntegral n / 2)) (primeFactors n)

problem3 :: (Integral a) => a
problem3 = largestPrimeFactor 600851475143
