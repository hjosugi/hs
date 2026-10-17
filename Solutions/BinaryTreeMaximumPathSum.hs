{-# LANGUAGE BangPatterns #-}

module Solutions.BinaryTreeMaximumPathSum where

import Common.Example (example)
import Data.Maybe (fromMaybe)

-- 問 45: 解答本体。main は末尾のサンプル入力で実行する。
-- BEGIN 45
data Tree = Empty | Node !Int Tree Tree
  deriving (Eq, Show)

maxPathSum :: Tree -> Maybe Int
maxPathSum = snd . gain
 where
  gain Empty = (0, Nothing)
  gain (Node value left right) =
    let (!lg, !lb) = gain left
        (!rg, !rb) = gain right
        !l = max 0 lg
        !r = max 0 rg
        !through = value + l + r
        !best = max (fromMaybe through lb) (max through (fromMaybe through rb))
        !up = value + max l r
     in (up, Just best)

-- END 45

main :: IO ()
main =
  example
    "Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)"
    (Just 6)
    (maxPathSum (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)))
