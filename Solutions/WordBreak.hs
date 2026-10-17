{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Solutions.WordBreak where

import Common.Arrays (newBools)
import Common.Example (example)
import Control.Monad (forM_, when)
import Control.Monad.ST (runST)
import qualified Data.Array as A
import Data.Array.MArray (readArray, writeArray)
import Data.List (foldl', tails)
import qualified Data.Map.Strict as M
import Solutions.ImplementTrie (Trie (..), emptyTrie, trieInsert)
import Prelude hiding (foldl')

-- 問 93: 解答本体。main は末尾のサンプル入力で実行する。
-- BEGIN 93
-- 辞書は Trie。文字列の全 suffix は tails で共有し、!! や毎回の drop を避ける。
wordBreak :: String -> [String] -> Bool
wordBreak s dictionary = runST $ do
  let n = length s
      root = foldl' (flip trieInsert) emptyTrie dictionary
  dp <- newBools (0, n) False
  writeArray dp 0 True
  let walk !_ _ [] = pure ()
      walk !end node (ch : rest) = case M.lookup ch (trieChildren node) of
        Nothing -> pure ()
        Just child -> do
          when (isWord child) $ writeArray dp (end + 1) True
          walk (end + 1) child rest
  forM_ (zip [0 ..] (tails s)) $ \(start, suffix) -> do
    reachable <- readArray dp start
    when reachable $ walk start root suffix
  readArray dp n

-- 追い質問: suffix から見た DP。辞書語が終わる位置を Trie で列挙する。
wordEnds :: Int -> String -> Trie -> [Int]
wordEnds !_ [] _ = []
wordEnds !i (ch : rest) node = case M.lookup ch (trieChildren node) of
  Nothing -> []
  Just child -> [i + 1 | isWord child] ++ wordEnds (i + 1) rest child

wordBreakOne :: String -> [String] -> Maybe [String]
wordBreakOne s dictionary = dp A.! 0
 where
  n = length s
  root = foldl' (flip trieInsert) emptyTrie dictionary
  dp =
    A.listArray
      (0, n)
      [ if i == n
          then Just []
          else
            firstJust
              [(take (j - i) suffix :) <$> (dp A.! j) | j <- wordEnds i suffix root]
      | (i, suffix) <- zip [0 ..] (tails s)
      ]
  firstJust [] = Nothing
  firstJust (Nothing : rest) = firstJust rest
  firstJust (result : _) = result

wordBreakCount :: String -> [String] -> Integer
wordBreakCount s dictionary = dp A.! 0
 where
  n = length s
  root = foldl' (flip trieInsert) emptyTrie dictionary
  dp =
    A.listArray
      (0, n)
      [ if i == n then 1 else sum [dp A.! j | j <- wordEnds i suffix root]
      | (i, suffix) <- zip [0 ..] (tails s)
      ]

-- END 93

main :: IO ()
main =
  example
    "wordBreak \"leetcode\" [\"leet\",\"code\"]"
    (True)
    (wordBreak "leetcode" ["leet", "code"])
