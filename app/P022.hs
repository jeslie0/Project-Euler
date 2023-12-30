module P022 where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Foldable
import Data.Word8 qualified as W8
import Data.Vector qualified as V
import Data.Vector.Algorithms.Tim qualified as T



name2Val :: B.ByteString -> Integer
name2Val name =
  B.foldl' (\prev curr -> if W8.isAlpha curr then prev + (fromIntegral (W8.toLower curr) - fromIntegral W8._a + 1) else prev) 0 name

indexedProduct :: V.Vector B.ByteString -> Integer
indexedProduct vec =
  V.ifoldl' (\prev index cur -> prev + (name2Val cur) * (fromIntegral index + 1)) 0 vec


p022 :: IO Integer
p022 = do
  datum <- B.readFile "./data/p022.txt"
  mVec <- V.unsafeThaw . V.fromList . B.split W8._comma $ datum
  T.sort mVec
  sortedVec <- V.unsafeFreeze mVec
  return $ indexedProduct sortedVec



main :: IO ()
main = p022 >>= print
