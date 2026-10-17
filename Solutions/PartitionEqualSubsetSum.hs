module Solutions.PartitionEqualSubsetSum where

import Common.Arrays (newBools)
import Common.Example (example)
import Control.Monad (forM_, when)
import Control.Monad.ST (runST)
import Data.Array.MArray (readArray, writeArray)

-- 問 95: 解答本体。main は末尾のサンプル入力で実行する。
-- BEGIN 95
canPartition :: [Int] -> Bool
canPartition nums
  | odd total = False
  | otherwise = runST $ do
      dp <- newBools (0, target) False
      writeArray dp 0 True
      forM_ nums $ \x -> forM_ [target, target - 1 .. x] $ \s -> do
        reachable <- readArray dp (s - x)
        when reachable $ writeArray dp s True
      readArray dp target
 where
  total = sum nums
  target = total `div` 2

-- END 95

main :: IO ()
main =
  example
    "canPartition [1,5,11,5]"
    (True)
    (canPartition [1, 5, 11, 5])
