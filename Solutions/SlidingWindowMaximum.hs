module Solutions.SlidingWindowMaximum where

import Common.Example (example)
import Data.List (foldl')
import qualified Data.Sequence as Q
import Prelude hiding (foldl')

-- 問 101: 解答本体。main は末尾のサンプル入力で実行する。
-- BEGIN 101
maxSlidingWindow :: [Int] -> Int -> [Int]
maxSlidingWindow nums k
  | k <= 0 = []
  | otherwise = reverse $ snd $ foldl' step (Q.empty, []) (zip [0 ..] nums)
 where
  expire i queue = case Q.viewl queue of
    (j, _) Q.:< rest | j <= i - k -> expire i rest
    _ -> queue
  trim x queue = case Q.viewr queue of
    rest Q.:> (_, value) | value < x -> trim x rest
    _ -> queue
  step (queue, answer) (i, x) =
    let queue' = trim x (expire i queue) Q.|> (i, x)
     in case Q.viewl queue' of
          (_, best) Q.:< _ | i >= k - 1 -> (queue', best : answer)
          _ -> (queue', answer)

-- END 101

main :: IO ()
main =
  example
    "maxSlidingWindow [1,3,-1,-3,5,3,6,7] 3"
    ([3, 3, 5, 5, 6, 7])
    (maxSlidingWindow [1, 3, -1, -3, 5, 3, 6, 7] 3)
