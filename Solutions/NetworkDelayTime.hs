{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Solutions.NetworkDelayTime where

import Common.Arrays (newInts)
import Common.Example (example)
import Common.Heap
import Control.Monad (foldM)
import Control.Monad.ST (runST)
import qualified Data.Array as A
import Data.Array.MArray (getElems, readArray, writeArray)

-- 問 82: 解答本体。main は末尾のサンプル入力で実行する。
-- BEGIN 82
networkDelayTime :: [(Int, Int, Int)] -> Int -> Int -> Int
networkDelayTime edges n source = runST $ do
  let graph =
        A.accumArray
          (flip (:))
          []
          (1, n)
          [(u, (v, w)) | (u, v, w) <- edges]
  distances <- newInts (1, n) maxBound
  writeArray distances source 0
  let relax !time heap (v, weight) = do
        old <- readArray distances v
        let !candidate = time + weight
        if candidate < old
          then writeArray distances v candidate >> pure (heapPush (candidate, v) heap)
          else pure heap
      loop heap = case heapPop heap of
        Nothing -> pure ()
        Just ((time, node), rest) -> do
          best <- readArray distances node
          if time /= best
            then loop rest
            else do
              next <- foldM (relax time) rest (graph A.! node)
              loop next
  loop (heapPush (0, source) HEmpty)
  ds <- getElems distances
  pure $ if maxBound `elem` ds then -1 else maximum ds

-- END 82

main :: IO ()
main =
  example
    "networkDelayTime [(2,1,1),(2,3,1),(3,4,1)] 4 2"
    (2)
    (networkDelayTime [(2, 1, 1), (2, 3, 1), (3, 4, 1)] 4 2)
