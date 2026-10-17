{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Solutions.DailyTemperatures where

import Common.Arrays (newInts)
import Common.Example (example)
import Control.Monad (foldM)
import Control.Monad.ST (runST)
import Data.Array.MArray (getElems, writeArray)

-- 問 21: 解答本体。main は末尾のサンプル入力で実行する。
-- BEGIN 21
dailyTemperatures :: [Int] -> [Int]
dailyTemperatures temperatures = runST $ do
  answer <- newInts (0, length temperatures - 1) 0
  let popColder !i !t ((j, old) : rest)
        | old < t = writeArray answer j (i - j) >> popColder i t rest
      popColder _ _ stack = pure stack
      step stack (i, t) = do
        rest <- popColder i t stack
        pure ((i, t) : rest)
  _ <- foldM step [] (zip [0 ..] temperatures)
  getElems answer

-- END 21

main :: IO ()
main =
  example
    "dailyTemperatures [73,74,75,71,69,72,76,73]"
    ([1, 1, 4, 2, 1, 1, 0, 0])
    (dailyTemperatures [73, 74, 75, 71, 69, 72, 76, 73])
