module Main where

import Prelude hiding (foldl')
import Control.Monad (forM_, unless)
import Data.List (foldl', sort, subsequences, tails)
import Must50DCore

check :: (Eq a, Show a) => String -> a -> a -> IO ()
check label expected actual = unless (expected == actual) $
  error (label ++ ": expected " ++ show expected ++ ", got " ++ show actual)

smallLists :: Int -> [a] -> [[a]]
smallLists n alphabet = concat [sequence (replicate k alphabet) | k <- [0 .. n]]

main :: IO ()
main = do
  check "61 touching/unsorted" [(1, 6), (8, 10)] $
    mergeIntervals [(8, 10), (2, 6), (1, 3), (6, 6)]
  check "64 sample" 2 $ minMeetingRooms [(0, 30), (5, 10), (15, 20)]
  check "64 reuse" 1 $ minMeetingRooms [(0, 10), (10, 20), (20, 30)]
  check "68 sample" 2 $ numIslands ["110", "001"]
  check "68 diagonals" 5 $ numIslands ["101", "010", "101"]
  check "68 empty" 0 $ numIslands []
  check "75 DAG" True $ canFinish 3 [(2, 1), (1, 0)]
  check "75 cycle" False $ canFinish 2 [(0, 1), (1, 0)]
  check "75 duplicate edge" True $ canFinish 2 [(1, 0), (1, 0)]
  check "75 empty" True $ canFinish 0 []
  check "80 sample" 5 $ ladderLength "hit" "cog" ["hot", "dot", "dog", "lot", "log", "cog"]
  check "80 no target" 0 $ ladderLength "hit" "cog" ["hot", "dot", "dog"]
  check "80 disconnected" 0 $ ladderLength "hit" "cog" ["cog"]
  check "82 sample" 2 $ networkDelayTime [(2, 1, 1), (2, 3, 1), (3, 4, 1)] 4 2
  check "82 stale entry" 2 $ networkDelayTime [(1, 2, 10), (1, 3, 1), (3, 2, 1)] 3 1
  check "82 unreachable" (-1) $ networkDelayTime [(1, 2, 1)] 2 2
  check "5 sample" [1, 2] $ topKFrequent [1, 1, 1, 2, 2, 3] 2
  check "14 sample" 3 $ lengthOfLongestSubstring "abcabcbb"
  check "14 left never retreats" 2 $ lengthOfLongestSubstring "abba"
  let trie = trieInsert "app" $ trieInsert "apple" emptyTrie
  check "108 exact" True $ trieSearch "app" trie
  check "108 prefix only" False $ trieSearch "appl" trie
  check "108 prefix" True $ trieStartsWith "appl" trie
  check "108 empty prefix" True $ trieStartsWith "" trie
  check "111 sample" ["eat", "oath"] $ sort $
    findWords ["oaan", "etae", "ihkr", "iflv"] ["oath", "pea", "eat", "rain"]
  check "111 restore/prune/prefix/deduplicate" ["a", "aa", "aaa", "aaaa"] $ sort $
    findWords ["aa", "aa"] ["a", "aa", "aaa", "aaaa", "aaaaa", "aa"]
  check "111 no cell reuse" [] $ findWords ["ab"] ["aba"]
  check "93 sample" True $ wordBreak "leetcode" ["leet", "code"]
  check "93 no split" False $ wordBreak "catsandog" ["cats", "dog", "sand", "and", "cat"]
  check "93 rebuild" (Just ["leet", "code"]) $ wordBreakOne "leetcode" ["leet", "code"]
  check "93 count/deduplicate" 5 $ wordBreakCount "aaaa" ["a", "aa", "aa"]
  check "86 sample" 12 $ rob [2, 7, 9, 3, 1]
  check "95 sample" True $ canPartition [1, 5, 11, 5]
  check "95 cannot reuse" False $ canPartition [1, 2, 5]
  check "94 sample" 4 $ lengthOfLIS [10, 9, 2, 5, 3, 7, 101, 18]
  check "45 sample" (Just 42) $
    maxPathSum (Node (-10) (Node 9 Empty Empty) (Node 20 (Node 15 Empty Empty) (Node 7 Empty Empty)))
  check "45 all negative" (Just (-2)) $ maxPathSum (Node (-3) (Node (-2) Empty Empty) Empty)
  check "45 empty" Nothing $ maxPathSum Empty
  check "21 sample" [1, 1, 4, 2, 1, 1, 0, 0] $ dailyTemperatures [73, 74, 75, 71, 69, 72, 76, 73]
  check "54 sample" [[2, 2, 3], [7]] $ combinationSum [2, 3, 6, 7] 7
  check "54 none" [] $ combinationSum [2] 1
  check "25 sample" 4 $ minEatingSpeed [3, 6, 7, 11] 8
  check "25 large total" 1000000000 $ minEatingSpeed (replicate 10000 1000000000) 10000
  check "79 triangle" (Just (2, 3)) $ findRedundantConnection [(1, 2), (1, 3), (2, 3)]
  check "79 path compression" (Just (1, 4)) $ findRedundantConnection [(1, 2), (2, 3), (3, 4), (1, 4), (1, 5)]
  check "267 sample" 3 $ subarraySum [1, 2, -1, 2, 1] 3
  check "267 zeros" 3 $ subarraySum [0, 0] 0
  check "268 sample" [[1, 1, 0], [1, 2, 1], [0, 1, 1]] $
    incrementSubmatrices 3 [(1, 1, 2, 2), (0, 0, 1, 1)]
  check "268 duplicate/boundary" [[2, 2], [2, 2]] $ incrementSubmatrices 2 (replicate 2 (0, 0, 1, 1))
  check "268 no rectangles" [[0, 0], [0, 0]] $ incrementSubmatrices 2 []
  let leaf = CutNode Nothing Nothing
      root = CutNode (Just (4, CutNode (Just (2, leaf)) (Just (3, leaf))))
                     (Just (7, CutNode (Just (1, leaf)) Nothing))
  check "270 binary sample" 5 $ minCutLeaves (Just root)
  check "270 root leaf" 0 $ minCutLeaves (Just leaf)
  check "270 large sum" 20000000000 $ minCutNary (NaryNode (replicate 20 (1000000000, NaryNode [])))
  check "270 take all negative edges" (-8) $
    minCutTakeNegatives (NaryNode [(-5, NaryNode [(-3, NaryNode []), (7, NaryNode [])])])

  -- ストリームを毎回ソートした結果と two-heaps を照合。重複・負数も含む。
  forM_ (smallLists 5 [-2, 0, 2]) $ \xs -> do
    let sorted = sort xs
        n = length xs
        expected | n == 0 = Nothing
                 | odd n = Just (fromIntegral (sorted !! (n `div` 2)))
                 | otherwise = Just ((fromIntegral (sorted !! (n `div` 2 - 1)) +
                                       fromIntegral (sorted !! (n `div` 2))) / 2)
    check "52 against sorting" expected $ findMedian (foldl' (flip addNum) emptyMedianFinder xs)

  -- 最適化した DP・deque・prefix count を、小さい入力の全探索と照合。
  forM_ (smallLists 5 [-1, 0, 1]) $ \xs -> do
    check "94 against subsequences" (maximum (0 : [length ys | ys <- subsequences xs,
      and (zipWith (<) ys (drop 1 ys))])) (lengthOfLIS xs)
    forM_ [-2 .. 2] $ \k ->
      check "267 against all subarrays" (length [() | i <- [0 .. length xs - 1],
        j <- [i + 1 .. length xs], sum (take (j - i) (drop i xs)) == k]) (subarraySum xs k)
    forM_ [1 .. length xs + 1] $ \k ->
      check "101 against windows" [maximum (take k (drop i xs)) | i <- [0 .. length xs - k]]
        (maxSlidingWindow xs k)
    check "21 against forward scan"
      [if any (> t) rest then length (takeWhile (<= t) rest) + 1 else 0 |
       (t, rest) <- zip xs (drop 1 (tails xs))] (dailyTemperatures xs)
  forM_ (smallLists 5 [1, 2, 3]) $ \xs ->
    check "95 against subsets" (even (sum xs) && any ((== sum xs `div` 2) . sum) (subsequences xs))
      (canPartition xs)

  -- 可変 API: 更新での使用順、miss、容量 0/1、既存キー上書きを確認。
  cache <- newLRUCache 2
  lruPut cache 1 1
  lruPut cache 2 2
  lruGet cache 1 >>= check "34 get refreshes" 1
  lruPut cache 3 3
  lruGet cache 2 >>= check "34 evicts oldest" (-1)
  lruPut cache 1 10
  lruPut cache 4 4
  lruGet cache 3 >>= check "34 update refreshes" (-1)
  lruGet cache 1 >>= check "34 updated value" 10
  single <- newLRUCache 1
  lruPut single 1 1
  lruPut single 2 2
  lruGet single 1 >>= check "34 capacity one" (-1)
  lruGet single 2 >>= check "34 tail/head repair" 2
  empty <- newLRUCache 0
  lruPut empty 1 1
  lruGet empty 1 >>= check "34 capacity zero" (-1)

  deduper <- newMessageDeduper 10 True
  results <- mapM (\t -> shouldPrint deduper t "solar") [10, 13, 20, 21, 31, 35]
  check "260 strict/hidden repeats" [True, False, False, True, False, True] results
  inclusive <- newMessageDeduper 10 False
  mapM (\t -> shouldPrint inclusive t "solar") [10, 20] >>= check "260 inclusive" [True, True]
  both <- newSuppressBothDeduper 10
  released <- mapM (\(t, message) -> suppressAdd both t message)
    [(0, "a"), (5, "b"), (10, "a"), (20, "a"), (31, "c")]
  check "260 suppress chains" [(5, "b")] (concat released)
  suppressFlush both >>= check "260 flush" [(31, "c")]
  suppressFlush both >>= check "260 flush twice" []

  bit <- newFenwick2D 3
  let rectangles = [(1, 1, 2, 2), (0, 0, 1, 1), (0, 0, 2, 2)]
  forM_ (zip [1..] rectangles) $ \(count, rectangle) -> do
    fenwickAddRect bit rectangle
    let expected = incrementSubmatrices 3 (take count rectangles)
    forM_ [(r, c) | r <- [0 .. 2], c <- [0 .. 2]] $ \pos@(r, c) ->
      fenwickQuery bit pos >>= check "268 online/offline" (expected !! r !! c)
  putStrLn "All 25 problems, bonus 260, and follow-ups passed."

