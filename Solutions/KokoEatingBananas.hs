{-# LANGUAGE BangPatterns #-}

module Solutions.KokoEatingBananas where

import Common.Example (example)
import Data.List (foldl')
import Prelude hiding (foldl')

-- 問 25: 解答本体。main は末尾のサンプル入力で実行する。
-- BEGIN 25
minEatingSpeed :: [Int] -> Int -> Int
minEatingSpeed [] _ = 0
minEatingSpeed piles hours = search 1 (maximum piles)
 where
  feasible speed =
    foldl' (\total p -> total + (toInteger p + toInteger speed - 1) `div` toInteger speed) 0 piles
      <= toInteger hours
  search !lo !hi
    | lo == hi = lo
    | feasible mid = search lo mid
    | otherwise = search (mid + 1) hi
   where
    mid = lo + (hi - lo) `div` 2

-- END 25

main :: IO ()
main =
  example
    "minEatingSpeed [3,6,7,11] 8"
    (4)
    (minEatingSpeed [3, 6, 7, 11] 8)
