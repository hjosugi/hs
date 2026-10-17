{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Solutions.IncrementSubmatrices where

import Common.Arrays (newGrid)
import Common.Example (example)
import Control.Monad (forM, forM_, when)
import Control.Monad.ST (runST)
import Data.Array.IO (IOUArray)
import Data.Array.MArray (newArray, readArray, writeArray)
import Data.Bits ((.&.))

-- 問 268: 解答本体。main は末尾のサンプル入力で実行する。
-- BEGIN 268
incrementSubmatrices :: Int -> [(Int, Int, Int, Int)] -> [[Int]]
incrementSubmatrices n rectangles = runST $ do
  diff <- newGrid ((0, 0), (n, n)) 0
  let add pos value = do
        old <- readArray diff pos
        writeArray diff pos (old + value)
  forM_ rectangles $ \(r1, c1, r2, c2) -> do
    add (r1, c1) 1
    add (r1, c2 + 1) (-1)
    add (r2 + 1, c1) (-1)
    add (r2 + 1, c2 + 1) 1
  forM_ [0 .. n - 1] $ \r -> forM_ [1 .. n - 1] $ \c ->
    readArray diff (r, c - 1) >>= add (r, c)
  forM_ [1 .. n - 1] $ \r -> forM_ [0 .. n - 1] $ \c ->
    readArray diff (r - 1, c) >>= add (r, c)
  forM [0 .. n - 1] $ \r -> forM [0 .. n - 1] $ \c -> readArray diff (r, c)

-- 追い質問: 矩形追加と 1 マスの問い合わせを交互に行う。
data Fenwick2D = Fenwick2D !Int (IOUArray (Int, Int) Int)

newFenwick2D :: Int -> IO Fenwick2D
newFenwick2D n = Fenwick2D n <$> newArray ((1, 1), (n, n)) 0

fenwickAdd :: Fenwick2D -> (Int, Int) -> Int -> IO ()
fenwickAdd (Fenwick2D n table) (r, c) value = outer (r + 1)
 where
  lowbit x = x .&. negate x
  outer i = when (i <= n) $ inner i (c + 1) >> outer (i + lowbit i)
  inner i j = when (j <= n) $ do
    old <- readArray table (i, j)
    writeArray table (i, j) (old + value)
    inner i (j + lowbit j)

fenwickAddRect :: Fenwick2D -> (Int, Int, Int, Int) -> IO ()
fenwickAddRect tree (r1, c1, r2, c2) = do
  fenwickAdd tree (r1, c1) 1
  fenwickAdd tree (r1, c2 + 1) (-1)
  fenwickAdd tree (r2 + 1, c1) (-1)
  fenwickAdd tree (r2 + 1, c2 + 1) 1

fenwickQuery :: Fenwick2D -> (Int, Int) -> IO Int
fenwickQuery (Fenwick2D _ table) (r, c) = outer (r + 1) 0
 where
  lowbit x = x .&. negate x
  outer 0 !total = pure total
  outer i !total = inner i (c + 1) total >>= outer (i - lowbit i)
  inner _ 0 !total = pure total
  inner i j !total = do
    value <- readArray table (i, j)
    inner i (j - lowbit j) (total + value)

-- END 268

main :: IO ()
main =
  example
    "incrementSubmatrices 3 [(1,1,2,2),(0,0,1,1)]"
    ([[1, 1, 0], [1, 2, 1], [0, 1, 1]])
    (incrementSubmatrices 3 [(1, 1, 2, 2), (0, 0, 1, 1)])
