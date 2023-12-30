module Problems.P022 where

import Criterion (Benchmark, bench, whnfIO)
import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Tim as T
import qualified Data.Word8 as W8

-- * Question

-- Using names.txt (right click and 'Save Link/Target As...'), a 46K
-- text file containing over five-thousand first names, begin by
-- sorting it into alphabetical order. Then working out the
-- alphabetical value for each name, multiply this value by its
-- alphabetical position in the list to obtain a name score.
--
-- For example, when the list is sorted into alphabetical order,
-- COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name
-- in the list. So, COLIN would obtain a score of 938 * 53 = 49714.
--
-- What is the total of all the name scores in the file?

-- * Solution

name2Val :: B.ByteString -> Int
name2Val name =
  B.foldl'
    ( \prev curr ->
        if W8.isAlpha curr
          then prev + (fromIntegral (W8.toLower curr) - fromIntegral W8._a + 1)
          else prev
    )
    0
    name

indexedProduct :: V.Vector B.ByteString -> Int
indexedProduct vec =
  V.ifoldl' (\prev index cur -> prev + (name2Val cur) * (fromIntegral index + 1)) 0 vec

p022 :: IO Int
p022 = do
  datum <- B.readFile "./data/p022.txt"
  mVec <- V.unsafeThaw . V.fromList . B.split W8._comma $ datum
  T.sort mVec
  sortedVec <- V.unsafeFreeze mVec
  return $ indexedProduct sortedVec

p022Benchmark :: Benchmark
p022Benchmark =
  bench "P022" $ whnfIO p022
