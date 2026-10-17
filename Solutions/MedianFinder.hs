module Solutions.MedianFinder where

import Common.Example (example)
import Common.Heap
import Data.Ord (Down (..))

-- 問 52: 解答本体。main は末尾のサンプル入力で実行する。
-- BEGIN 52
data MedianFinder = MedianFinder (Heap (Down Int)) (Heap Int)
  deriving (Show)

emptyMedianFinder :: MedianFinder
emptyMedianFinder = MedianFinder HEmpty HEmpty

addNum :: Int -> MedianFinder -> MedianFinder
addNum num (MedianFinder low high) =
  case heapPop (heapPush (Down num) low) of
    Nothing -> error "addNum: impossible empty heap"
    Just (Down x, low') ->
      let high' = heapPush x high
       in if heapSize high' <= heapSize low'
            then MedianFinder low' high'
            else case heapPop high' of
              Nothing -> error "addNum: impossible empty high heap"
              Just (y, high'') -> MedianFinder (heapPush (Down y) low') high''

findMedian :: MedianFinder -> Maybe Double
findMedian (MedianFinder low high) = do
  Down x <- heapPeek low
  if heapSize low > heapSize high
    then pure (fromIntegral x)
    else do
      y <- heapPeek high
      pure ((fromIntegral x + fromIntegral y) / 2)

-- END 52

main :: IO ()
main =
  example
    "addNum 1, addNum 2, addNum 3; findMedian"
    (Just 2.0)
    (findMedian (addNum 3 (addNum 2 (addNum 1 emptyMedianFinder))))
