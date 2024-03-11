module ProjectEuler.Primes where

import qualified Data.PQueue.Prio.Min as P

-- * Primes
-- This is taken almost verbatim from Melissa O'Neil's Genuine Sieve
-- of Eratosthenes paper.

sieve [] = []
sieve (x:xs) = x : sieve' xs (insertPrime x xs P.empty)
  where
    insertPrime p xs = P.insert (p * p) (map (* p) xs)
    sieve' [] table     = []
    sieve' (x:xs) table
      | nextComposite <= x = sieve' xs (adjust table)
      | otherwise          = x : sieve' xs (insertPrime x xs table)
      where
        nextComposite = fst $ P.findMin table
        adjust table
          | n <= x    = adjust (deleteMinAndInsert n' ns table)
          | otherwise = table
          where
            (n, n':ns) = P.findMin table
            deleteMinAndInsert :: (Ord k) => k -> v -> P.MinPQueue k v -> P.MinPQueue k v
            deleteMinAndInsert k v table = P.insert k v $ P.deleteMin table

wheel2357 :: Integral a => [a]
wheel2357 =
  2:4:2:4:6:2:6:4:2:4:6:6:2:6:4:2:6:4:6:8:4:2:4:2:4:8:6:4:6:2:4:6:2:6:6:4:2:4:6:2:6:4:2:4:2:10:2:10:wheel2357

spin :: Num t => [t] -> t -> [t]
spin (x:xs) n = n : spin xs (n + x)

primes :: Integral a => [a]
primes = 2:3:5:7: sieve (spin wheel2357 11)


-- * Using the list of primes

-- | Generates a list of prime factors of n, with repetition.
primeFactors :: Integral a => a -> [a]
primeFactors n = divtest n primes
  where
    divtest 1 _ = []
    divtest _ [] = []
    divtest n (x:xs)
      | n `mod` x == 0 = x : divtest (n `div` x) (x:xs)
      | otherwise      = divtest n xs

-- | Generates a list of primes, with their corresponding factors
primeFactorPowers :: Integral a => a -> [(a, Int)]
primeFactorPowers = collect . primeFactors
  where
    collect :: (Eq a) => [a] -> [(a,Int)]
    collect [] = []
    collect xs =
      let (ns, remaining) = span (\a -> a == head xs) xs
      in (head ns, length ns) : collect remaining

-- | Converts a list of prime powers to a number. This is the inverse
-- of the above.
primePowers2Int :: Integral a => [(a, Int)] -> a
primePowers2Int [] = 1
primePowers2Int ((n,i):ns) = n ^ i * primePowers2Int ns

-- * Factors

listOfFactors :: (Integral a) => a -> [a]
listOfFactors n = listOfFactors' 2 [1]
  where
    listOfFactors' m acc
      | m * m > n  = acc
      | n `rem` m == 0 = listOfFactors' (m+1) (n `div` m : if m * m == n then acc else m : acc)
      | otherwise      = listOfFactors' (m+1) acc

numberOfFactors :: (Integral a) => a -> Int
numberOfFactors n = product $ map ((+1) . snd) $ primeFactorPowers n
