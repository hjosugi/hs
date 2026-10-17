{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Solutions.NumberOfIslands where

import Common.Arrays (newBools)
import Common.Example (example)
import Control.Monad (foldM, forM_)
import Control.Monad.ST (runST)
import Data.Array.MArray (readArray, writeArray)

-- 問 68: 解答本体。main は末尾のサンプル入力で実行する。
-- BEGIN 68
numIslands :: [String] -> Int
numIslands [] = 0
numIslands rows@(first : _)
  | null first = 0
  | otherwise = runST $ do
      let m = length rows
          n = length first
          neighbors i =
            let (r, c) = i `divMod` n
             in [ r' * n + c'
                | (r', c') <- [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)]
                , 0 <= r'
                , r' < m
                , 0 <= c'
                , c' < n
                ]
      land <- newBools (0, m * n - 1) False
      forM_ (zip [0 ..] (concat rows)) $ \(i, ch) ->
        writeArray land i (ch == '1')
      let sink [] = pure ()
          sink (i : stack) = do
            next <-
              foldM
                ( \acc j -> do
                    alive <- readArray land j
                    if alive
                      then writeArray land j False >> pure (j : acc)
                      else pure acc
                )
                stack
                (neighbors i)
            sink next
      foldM
        ( \count i -> do
            alive <- readArray land i
            if alive
              then do
                writeArray land i False -- stack に積む時点で訪問済みにする
                sink [i]
                pure $! count + 1
              else pure count
        )
        0
        [0 .. m * n - 1]

-- END 68

main :: IO ()
main =
  example
    "numIslands [\"110\",\"001\"]"
    (2)
    (numIslands ["110", "001"])
