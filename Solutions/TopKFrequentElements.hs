module Solutions.TopKFrequentElements where

import Common.Example (example)
import Control.Monad (forM_)
import Control.Monad.ST (runST)
import qualified Data.Array as A
import qualified Data.HashTable.Class as HC
import qualified Data.HashTable.ST.Basic as HT
import Data.Maybe (fromMaybe)

-- 問 5: 解答本体。main は末尾のサンプル入力で実行する。
-- BEGIN 5
topKFrequent :: [Int] -> Int -> [Int]
topKFrequent nums k = runST $ do
  counts <- HT.new
  forM_ nums $ \x -> do
    count <- fromMaybe 0 <$> HT.lookup counts x
    HT.insert counts x $! count + 1
  frequencies <- HC.toList counts
  let n = length nums
      buckets =
        A.accumArray
          (flip (:))
          []
          (0, n)
          [(count, x) | (x, count) <- frequencies]
  pure $ take k $ concat [buckets A.! count | count <- [n, n - 1 .. 1]]

-- END 5

main :: IO ()
main =
  example
    "topKFrequent [1,1,1,2,2,3] 2"
    ([1, 2])
    (topKFrequent [1, 1, 1, 2, 2, 3] 2)
