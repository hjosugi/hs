{-# LANGUAGE BangPatterns #-}

module Solutions.HouseRobber where

import Common.Example (example)
import Data.List (foldl')
import Prelude hiding (foldl')

-- 問 86: 解答本体。main は末尾のサンプル入力で実行する。
-- BEGIN 86
rob :: [Int] -> Int
rob = snd . foldl' step (0, 0)
 where
  step (!prev2, !prev1) x =
    let !best = max prev1 (prev2 + x)
     in (prev1, best)

-- END 86

main :: IO ()
main =
  example
    "rob [2,7,9,3,1]"
    (12)
    (rob [2, 7, 9, 3, 1])
