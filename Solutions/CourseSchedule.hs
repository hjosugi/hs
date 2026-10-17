{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Solutions.CourseSchedule where

import Common.Arrays (newInts)
import Common.Example (example)
import Control.Monad (filterM, foldM, forM_)
import Control.Monad.ST (runST)
import qualified Data.Array as A
import Data.Array.MArray (readArray, writeArray)
import qualified Data.Sequence as Q

-- 問 75: 解答本体。main は末尾のサンプル入力で実行する。
-- BEGIN 75
canFinish :: Int -> [(Int, Int)] -> Bool
canFinish n prerequisites = runST $ do
  let graph =
        A.accumArray
          (flip (:))
          []
          (0, n - 1)
          [(pre, course) | (course, pre) <- prerequisites]
  indegree <- newInts (0, n - 1) 0
  forM_ prerequisites $ \(course, _) -> do
    d <- readArray indegree course
    writeArray indegree course (d + 1)
  starts <- filterM (\i -> (== 0) <$> readArray indegree i) [0 .. n - 1]
  let loop !seen queue = case Q.viewl queue of
        Q.EmptyL -> pure (seen == n)
        node Q.:< rest -> do
          next <-
            foldM
              ( \q v -> do
                  d <- readArray indegree v
                  writeArray indegree v (d - 1)
                  pure $ if d == 1 then q Q.|> v else q
              )
              rest
              (graph A.! node)
          loop (seen + 1) next
  loop 0 (Q.fromList starts)

-- END 75

main :: IO ()
main =
  example
    "canFinish 3 [(2,1),(1,0)]"
    (True)
    (canFinish 3 [(2, 1), (1, 0)])
