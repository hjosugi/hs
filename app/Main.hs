module Main where

import Must50DCore

main :: IO ()
main = do
  putStrLn "Must 50 Haskell examples"
  putStrLn $ "61  Merge Intervals: " ++ show (mergeIntervals [(1, 3), (2, 6), (8, 10)])
  putStrLn $ "68  Number of Islands: " ++ show (numIslands ["110", "001"])
  putStrLn $ "93  Word Break: " ++ show (wordBreak "leetcode" ["leet", "code"])
  putStrLn $ "86  House Robber: " ++ show (rob [2, 7, 9, 3, 1])
  putStrLn $ "94  Longest Increasing Subsequence: " ++ show (lengthOfLIS [10, 9, 2, 5, 3, 7, 101, 18])
  let median = addNum 3 $ addNum 2 $ addNum 1 emptyMedianFinder
  putStrLn $ "52  Median: " ++ show (findMedian median)
  cache <- newLRUCache 2
  lruPut cache 1 10
  lruPut cache 2 20
  value <- lruGet cache 1
  putStrLn $ "34  LRU get(1): " ++ show value
  putStrLn $ "267 Subarray Sum Equals K: " ++ show (subarraySum [1, 2, -1, 2, 1] 3)
