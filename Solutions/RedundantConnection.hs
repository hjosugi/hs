{-# LANGUAGE FlexibleContexts #-}

module Solutions.RedundantConnection where

import Common.Arrays (newInts)
import Common.Example (example)
import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Data.Array.MArray (readArray, writeArray)

-- 問 79: 解答本体。main は末尾のサンプル入力で実行する。
-- BEGIN 79
findRedundantConnection :: [(Int, Int)] -> Maybe (Int, Int)
findRedundantConnection edges = runST $ do
  let n = length edges -- LC の節点ラベルは 1..n
  parent <- newInts (0, n) 0
  size <- newInts (0, n) 1
  forM_ [0 .. n] $ \i -> writeArray parent i i
  let find x = do
        p <- readArray parent x
        if p == x
          then pure x
          else do
            root <- find p
            writeArray parent x root -- path compression
            pure root
      loop [] = pure Nothing
      loop ((a, b) : rest) = do
        ra <- find a
        rb <- find b
        if ra == rb
          then pure (Just (a, b))
          else do
            sa <- readArray size ra
            sb <- readArray size rb
            let (small, big) = if sa < sb then (ra, rb) else (rb, ra)
            writeArray parent small big
            writeArray size big (sa + sb) -- union by size
            loop rest
  loop edges

-- END 79

main :: IO ()
main =
  example
    "findRedundantConnection [(1,2),(1,3),(2,3)]"
    (Just (2, 3))
    (findRedundantConnection [(1, 2), (1, 3), (2, 3)])
