{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Solutions.LongestIncreasingSubsequence where

import Common.Arrays (newInts)
import Common.Example (example)
import Control.Monad (foldM)
import Control.Monad.ST (runST)
import Data.Array.MArray (readArray, writeArray)

-- 問 94: 解答本体。main は末尾のサンプル入力で実行する。
-- BEGIN 94
lengthOfLIS :: [Int] -> Int
lengthOfLIS nums = runST $ do
  smallest <- newInts (0, length nums - 1) 0
  let lowerBound !x !lo !hi
        | lo == hi = pure lo
        | otherwise = do
            let mid = lo + (hi - lo) `div` 2
            value <- readArray smallest mid
            if value < x then lowerBound x (mid + 1) hi else lowerBound x lo mid
      step !size x = do
        i <- lowerBound x 0 size
        writeArray smallest i x
        pure $! if i == size then size + 1 else size
  foldM step 0 nums

-- END 94

main :: IO ()
main =
  example
    "lengthOfLIS [10,9,2,5,3,7,101,18]"
    (4)
    (lengthOfLIS [10, 9, 2, 5, 3, 7, 101, 18])
