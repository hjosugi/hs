module Solutions.CombinationSum where

import Common.Example (example)
import Data.List (sort)

-- 問 54: 解答本体。main は末尾のサンプル入力で実行する。
-- BEGIN 54
combinationSum :: [Int] -> Int -> [[Int]]
combinationSum candidates target = go target (sort candidates)
 where
  go 0 _ = [[]]
  go _ [] = []
  go remaining choices@(x : rest)
    | x > remaining = []
    | otherwise = map (x :) (go (remaining - x) choices) ++ go remaining rest

-- END 54

main :: IO ()
main =
  example
    "combinationSum [2,3,6,7] 7"
    ([[2, 2, 3], [7]])
    (combinationSum [2, 3, 6, 7] 7)
