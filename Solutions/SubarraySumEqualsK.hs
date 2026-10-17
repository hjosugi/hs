{-# LANGUAGE BangPatterns #-}

module Solutions.SubarraySumEqualsK where

import Common.Example (example)
import Control.Monad.ST (runST)
import qualified Data.HashTable.ST.Basic as HT
import Data.Maybe (fromMaybe)

-- 問 267: 解答本体。main は末尾のサンプル入力で実行する。
-- BEGIN 267
subarraySum :: [Int] -> Int -> Int
subarraySum nums k = runST $ do
  seen <- HT.new
  HT.insert seen 0 (1 :: Int)
  let loop !_ !answer [] = pure answer
      loop !prefix !answer (x : rest) = do
        let !prefix' = prefix + x
        matches <- fromMaybe 0 <$> HT.lookup seen (prefix' - k)
        count <- fromMaybe 0 <$> HT.lookup seen prefix'
        HT.insert seen prefix' $! count + 1
        loop prefix' (answer + matches) rest
  loop 0 0 nums

-- END 267

main :: IO ()
main =
  example
    "subarraySum [1,2,-1,2,1] 3"
    (3)
    (subarraySum [1, 2, -1, 2, 1] 3)
