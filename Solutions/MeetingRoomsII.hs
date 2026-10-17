module Solutions.MeetingRoomsII where

import Common.Example (example)
import Common.Heap
import Data.List (foldl', sort)
import Prelude hiding (foldl')

-- 問 64: 解答本体。main は末尾のサンプル入力で実行する。
-- BEGIN 64
minMeetingRooms :: [(Int, Int)] -> Int
minMeetingRooms = heapSize . foldl' step HEmpty . sort
 where
  step heap (s, e) =
    case heapPop heap of
      Just (end, rest) | end <= s -> heapPush e rest
      _ -> heapPush e heap

-- END 64

main :: IO ()
main =
  example
    "minMeetingRooms [(0,30),(5,10),(15,20)]"
    (2)
    (minMeetingRooms [(0, 30), (5, 10), (15, 20)])
