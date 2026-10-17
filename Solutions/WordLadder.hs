{-# LANGUAGE BangPatterns #-}

module Solutions.WordLadder where

import Common.Example (example)
import Control.Monad (foldM, forM_)
import Control.Monad.ST (runST)
import qualified Data.HashTable.ST.Basic as HT
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Sequence as Q

-- 問 80: 解答本体。main は末尾のサンプル入力で実行する。
-- BEGIN 80
ladderLength :: String -> String -> [String] -> Int
ladderLength begin end wordsList
  | end `notElem` wordsList = 0
  | otherwise = runST $ do
      buckets <- HT.new
      visited <- HT.new
      let patterns w = [take i w ++ "*" ++ drop (i + 1) w | i <- [0 .. length w - 1]]
      forM_ wordsList $ \w -> forM_ (patterns w) $ \p -> do
        old <- fromMaybe [] <$> HT.lookup buckets p
        HT.insert buckets p (w : old)
      HT.insert visited begin ()
      let visit !steps queue w = do
            seen <- HT.lookup visited w
            if isJust seen
              then pure queue
              else do
                HT.insert visited w ()
                pure (queue Q.|> (w, steps + 1))
          expand !steps queue p = do
            neighbors <- fromMaybe [] <$> HT.lookup buckets p
            HT.delete buckets p -- 同じ bucket を何度も走査しない
            foldM (visit steps) queue neighbors
          bfs queue = case Q.viewl queue of
            Q.EmptyL -> pure 0
            (word, steps) Q.:< rest
              | word == end -> pure steps
              | otherwise -> foldM (expand steps) rest (patterns word) >>= bfs
      bfs (Q.singleton (begin, 1))

-- END 80

main :: IO ()
main =
  example
    "ladderLength \"hit\" \"cog\" [\"hot\",\"dot\",\"dog\",\"lot\",\"log\",\"cog\"]"
    (5)
    (ladderLength "hit" "cog" ["hot", "dot", "dog", "lot", "log", "cog"])
