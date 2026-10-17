module Solutions.MergeIntervals where

import Common.Example (example)
import Data.List (foldl', sort)
import Prelude hiding (foldl')

-- 問 61: 解答本体。main は末尾のサンプル入力で実行する。
-- BEGIN 61
mergeIntervals :: [(Int, Int)] -> [(Int, Int)]
mergeIntervals = reverse . foldl' step [] . sort
 where
  step ((a, b) : rest) (s, e)
    | s <= b = (a, max b e) : rest
  step acc interval = interval : acc

-- END 61

main :: IO ()
main =
  example
    "mergeIntervals [(1,3),(2,6),(8,10)]"
    ([(1, 6), (8, 10)])
    (mergeIntervals [(1, 3), (2, 6), (8, 10)])
