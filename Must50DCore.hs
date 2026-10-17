{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Must50DCore where

import Prelude hiding (foldl')
import Control.Monad (filterM, foldM, forM, forM_, when)
import Control.Monad.ST (ST, runST)
import qualified Data.Array as A
import Data.Array.IO (IOUArray)
import Data.Array.MArray (getElems, newArray, newListArray, readArray, writeArray)
import Data.Array.ST (STUArray)
import Data.Bits ((.&.))
import qualified Data.HashTable.Class as HC
import qualified Data.HashTable.IO as H
import qualified Data.HashTable.ST.Basic as HT
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (foldl', sort, tails)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import Data.Ord (Down (..))
import qualified Data.Sequence as Q
import Data.STRef (modifySTRef', newSTRef, readSTRef)

-- BEGIN common
-- 共通: 重複を保持できる leftist min-heap。Down を使うと max-heap。
data Heap a = HEmpty | HNode !Int !Int a (Heap a) (Heap a)
  deriving (Show)

heapRank :: Heap a -> Int
heapRank HEmpty = 0
heapRank (HNode r _ _ _ _) = r

heapSize :: Heap a -> Int
heapSize HEmpty = 0
heapSize (HNode _ n _ _ _) = n

heapNode :: a -> Heap a -> Heap a -> Heap a
heapNode x a b
  | heapRank a < heapRank b = heapNode x b a
  | otherwise = HNode (heapRank b + 1) (1 + heapSize a + heapSize b) x a b

meld :: Ord a => Heap a -> Heap a -> Heap a
meld HEmpty b = b
meld a HEmpty = a
meld a@(HNode _ _ x l r) b@(HNode _ _ y _ _)
  | x <= y = heapNode x l (meld r b)
  | otherwise = meld b a

heapPush :: Ord a => a -> Heap a -> Heap a
heapPush x = meld (HNode 1 1 x HEmpty HEmpty)

heapPeek :: Heap a -> Maybe a
heapPeek HEmpty = Nothing
heapPeek (HNode _ _ x _ _) = Just x

heapPop :: Ord a => Heap a -> Maybe (a, Heap a)
heapPop HEmpty = Nothing
heapPop (HNode _ _ x l r) = Just (x, meld l r)

newInts :: (Int, Int) -> Int -> ST s (STUArray s Int Int)
newInts = newArray

newBools :: (Int, Int) -> Bool -> ST s (STUArray s Int Bool)
newBools = newArray

newGrid :: ((Int, Int), (Int, Int)) -> Int -> ST s (STUArray s (Int, Int) Int)
newGrid = newArray

newBoard :: ((Int, Int), (Int, Int)) -> [Char] -> ST s (STUArray s (Int, Int) Char)
newBoard = newListArray
-- END common

-- BEGIN 61
mergeIntervals :: [(Int, Int)] -> [(Int, Int)]
mergeIntervals = reverse . foldl' step [] . sort
  where
    step ((a, b) : rest) (s, e)
      | s <= b = (a, max b e) : rest
    step acc interval = interval : acc
-- END 61

-- BEGIN 64
minMeetingRooms :: [(Int, Int)] -> Int
minMeetingRooms = heapSize . foldl' step HEmpty . sort
  where
    step heap (s, e) =
      case heapPop heap of
        Just (end, rest) | end <= s -> heapPush e rest
        _ -> heapPush e heap
-- END 64

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
            in [r' * n + c' | (r', c') <- [(r+1,c),(r-1,c),(r,c+1),(r,c-1)],
                              0 <= r', r' < m, 0 <= c', c' < n]
      land <- newBools (0, m * n - 1) False
      forM_ (zip [0..] (concat rows)) $ \(i, ch) ->
        writeArray land i (ch == '1')
      let sink [] = pure ()
          sink (i : stack) = do
            next <- foldM (\acc j -> do
              alive <- readArray land j
              if alive
                then writeArray land j False >> pure (j : acc)
                else pure acc) stack (neighbors i)
            sink next
      foldM (\count i -> do
        alive <- readArray land i
        if alive
          then do
            writeArray land i False -- stack に積む時点で訪問済みにする
            sink [i]
            pure $! count + 1
          else pure count) 0 [0 .. m * n - 1]
-- END 68

-- BEGIN 260
-- 元資料の本文にだけ入っている補足問題。
data MessageDeduper = MessageDeduper !Int !Bool (H.BasicHashTable String Int)

newMessageDeduper :: Int -> Bool -> IO MessageDeduper
newMessageDeduper window strict = MessageDeduper window strict <$> H.new

shouldPrint :: MessageDeduper -> Int -> String -> IO Bool
shouldPrint (MessageDeduper window strict lastShown) timestamp message = do
  previous <- H.lookup lastShown message
  let tooSoon lastTime =
        if strict then timestamp - lastTime <= window else timestamp - lastTime < window
  if maybe False tooSoon previous
    then pure False -- 隠した出現では lastShown を更新しない
    else H.insert lastShown message timestamp >> pure True

data PendingMessage = PendingMessage !Int String (IORef Bool)

data SuppressBothDeduper = SuppressBothDeduper
  { suppressWindow :: !Int
  , suppressPending :: IORef (Q.Seq PendingMessage)
  , suppressLatest :: H.BasicHashTable String PendingMessage
  }

newSuppressBothDeduper :: Int -> IO SuppressBothDeduper
newSuppressBothDeduper window = SuppressBothDeduper window <$> newIORef Q.empty <*> H.new

suppressAdd :: SuppressBothDeduper -> Int -> String -> IO [(Int, String)]
suppressAdd deduper timestamp message = do
  released <- suppressRelease deduper (Just timestamp)
  flag <- newIORef False
  previous <- H.lookup (suppressLatest deduper) message
  forM_ previous $ \(PendingMessage lastTime _ previousFlag) ->
    when (timestamp - lastTime <= suppressWindow deduper) $ do
      writeIORef previousFlag True
      writeIORef flag True
  let entry = PendingMessage timestamp message flag
  modifyIORef' (suppressPending deduper) (Q.|> entry)
  H.insert (suppressLatest deduper) message entry
  pure released

suppressFlush :: SuppressBothDeduper -> IO [(Int, String)]
suppressFlush deduper = suppressRelease deduper Nothing

suppressRelease :: SuppressBothDeduper -> Maybe Int -> IO [(Int, String)]
suppressRelease deduper now = do
  pending <- readIORef (suppressPending deduper)
  let loop queue output = case Q.viewl queue of
        PendingMessage timestamp message flag Q.:< rest
          | maybe True (timestamp + suppressWindow deduper <) now -> do
              suppressed <- readIORef flag
              latest <- H.lookup (suppressLatest deduper) message
              case latest of
                Just (PendingMessage _ _ latestFlag) | latestFlag == flag ->
                  H.delete (suppressLatest deduper) message
                _ -> pure ()
              loop rest (if suppressed then output else (timestamp, message) : output)
        _ -> do
          writeIORef (suppressPending deduper) queue
          pure (reverse output)
  loop pending []
-- END 260

-- BEGIN 75
canFinish :: Int -> [(Int, Int)] -> Bool
canFinish n prerequisites = runST $ do
  let graph = A.accumArray (flip (:)) [] (0, n - 1)
                [(pre, course) | (course, pre) <- prerequisites]
  indegree <- newInts (0, n - 1) 0
  forM_ prerequisites $ \(course, _) -> do
    d <- readArray indegree course
    writeArray indegree course (d + 1)
  starts <- filterM (\i -> (== 0) <$> readArray indegree i) [0 .. n - 1]
  let loop !seen queue = case Q.viewl queue of
        Q.EmptyL -> pure (seen == n)
        node Q.:< rest -> do
          next <- foldM (\q v -> do
            d <- readArray indegree v
            writeArray indegree v (d - 1)
            pure $ if d == 1 then q Q.|> v else q) rest (graph A.! node)
          loop (seen + 1) next
  loop 0 (Q.fromList starts)

-- END 75

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
            if isJust seen then pure queue else do
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

-- BEGIN 82
networkDelayTime :: [(Int, Int, Int)] -> Int -> Int -> Int
networkDelayTime edges n source = runST $ do
  let graph = A.accumArray (flip (:)) [] (1, n)
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
          if time /= best then loop rest else do
            next <- foldM (relax time) rest (graph A.! node)
            loop next
  loop (heapPush (0, source) HEmpty)
  ds <- getElems distances
  pure $ if maxBound `elem` ds then -1 else maximum ds
-- END 82

-- BEGIN 34
-- IORef によりノードを O(1) で付け替える。map は hashtables の可変 hash table。
data CacheNode = CacheNode
  { cacheKey :: !Int
  , cacheValue :: IORef Int
  , cachePrev :: IORef (Maybe CacheNode)
  , cacheNext :: IORef (Maybe CacheNode)
  }

data LRUCache = LRUCache
  { cacheCapacity :: !Int
  , cacheMap :: H.BasicHashTable Int CacheNode
  , cacheHead :: IORef (Maybe CacheNode)
  , cacheTail :: IORef (Maybe CacheNode)
  , cacheSize :: IORef Int
  }

newLRUCache :: Int -> IO LRUCache
newLRUCache capacity =
  LRUCache (max 0 capacity) <$> H.new <*> newIORef Nothing <*> newIORef Nothing <*> newIORef 0

detach :: LRUCache -> CacheNode -> IO ()
detach cache node = do
  previous <- readIORef (cachePrev node)
  next <- readIORef (cacheNext node)
  maybe (writeIORef (cacheHead cache) next)
        (\p -> writeIORef (cacheNext p) next) previous
  maybe (writeIORef (cacheTail cache) previous)
        (\q -> writeIORef (cachePrev q) previous) next

addFront :: LRUCache -> CacheNode -> IO ()
addFront cache node = do
  first <- readIORef (cacheHead cache)
  writeIORef (cachePrev node) Nothing
  writeIORef (cacheNext node) first
  maybe (writeIORef (cacheTail cache) (Just node))
        (\p -> writeIORef (cachePrev p) (Just node)) first
  writeIORef (cacheHead cache) (Just node)

lruGet :: LRUCache -> Int -> IO Int
lruGet cache key = do
  found <- H.lookup (cacheMap cache) key
  case found of
    Nothing -> pure (-1)
    Just node -> do
      detach cache node
      addFront cache node
      readIORef (cacheValue node)

lruPut :: LRUCache -> Int -> Int -> IO ()
lruPut cache key value = when (cacheCapacity cache > 0) $ do
  found <- H.lookup (cacheMap cache) key
  case found of
    Just node -> do
      writeIORef (cacheValue node) value
      detach cache node
      addFront cache node
    Nothing -> do
      size <- readIORef (cacheSize cache)
      if size < cacheCapacity cache
        then modifyIORef' (cacheSize cache) (+ 1)
        else do
          oldest <- readIORef (cacheTail cache)
          forM_ oldest $ \node -> do
            detach cache node
            H.delete (cacheMap cache) (cacheKey node)
      node <- CacheNode key <$> newIORef value <*> newIORef Nothing <*> newIORef Nothing
      addFront cache node
      H.insert (cacheMap cache) key node
-- END 34

-- BEGIN 5
topKFrequent :: [Int] -> Int -> [Int]
topKFrequent nums k = runST $ do
  counts <- HT.new
  forM_ nums $ \x -> do
    count <- fromMaybe 0 <$> HT.lookup counts x
    HT.insert counts x $! count + 1
  frequencies <- HC.toList counts
  let n = length nums
      buckets = A.accumArray (flip (:)) [] (0, n)
                  [(count, x) | (x, count) <- frequencies]
  pure $ take k $ concat [buckets A.! count | count <- [n, n - 1 .. 1]]
-- END 5

-- BEGIN 52
data MedianFinder = MedianFinder (Heap (Down Int)) (Heap Int)
  deriving (Show)

emptyMedianFinder :: MedianFinder
emptyMedianFinder = MedianFinder HEmpty HEmpty

addNum :: Int -> MedianFinder -> MedianFinder
addNum num (MedianFinder low high) =
  case heapPop (heapPush (Down num) low) of
    Nothing -> error "addNum: impossible empty heap"
    Just (Down x, low') ->
      let high' = heapPush x high
      in if heapSize high' <= heapSize low'
           then MedianFinder low' high'
           else case heapPop high' of
             Nothing -> error "addNum: impossible empty high heap"
             Just (y, high'') -> MedianFinder (heapPush (Down y) low') high''

findMedian :: MedianFinder -> Maybe Double
findMedian (MedianFinder low high) = do
  Down x <- heapPeek low
  if heapSize low > heapSize high
    then pure (fromIntegral x)
    else do
      y <- heapPeek high
      pure ((fromIntegral x + fromIntegral y) / 2)
-- END 52

-- BEGIN 14
lengthOfLongestSubstring :: String -> Int
lengthOfLongestSubstring s = runST $ do
  lastSeen <- HT.new
  let loop !_ !_ !best [] = pure best
      loop !right !left !best (ch : rest) = do
        previous <- HT.lookup lastSeen ch
        let !left' = maybe left (max left . (+ 1)) previous
        HT.insert lastSeen ch right
        loop (right + 1) left' (max best (right - left' + 1)) rest
  loop 0 0 0 s
-- END 14

-- BEGIN 108
data Trie = Trie
  { isWord :: !Bool
  , trieChildren :: M.Map Char Trie
  } deriving (Show)

emptyTrie :: Trie
emptyTrie = Trie False M.empty

trieInsert :: String -> Trie -> Trie
trieInsert [] (Trie _ children) = Trie True children
trieInsert (ch : rest) (Trie terminal children) =
  Trie terminal (M.alter (Just . trieInsert rest . fromMaybe emptyTrie) ch children)

trieWalk :: String -> Trie -> Maybe Trie
trieWalk [] node = Just node
trieWalk (ch : rest) (Trie _ children) = M.lookup ch children >>= trieWalk rest

trieSearch :: String -> Trie -> Bool
trieSearch word = maybe False isWord . trieWalk word

trieStartsWith :: String -> Trie -> Bool
trieStartsWith prefix = isJust . trieWalk prefix
-- END 108

-- BEGIN 111
findWords :: [String] -> [String] -> [String]
findWords [] _ = []
findWords rows@(first : _) wordsList
  | null first = []
  | otherwise = runST $ do
      let m = length rows
          n = length first
          cells = [(r, c) | r <- [0 .. m - 1], c <- [0 .. n - 1]]
          root = foldl' (flip trieInsert) emptyTrie wordsList
      board <- newBoard ((0, 0), (m - 1, n - 1)) (concat rows)
      found <- newSTRef []
      let dfs path node@(Trie terminal children) (r, c) = do
            ch <- readArray board (r, c)
            case M.lookup ch children of
              Nothing -> pure node
              Just (Trie word next) -> do
                let path' = ch : path
                when word $ modifySTRef' found (reverse path' :)
                writeArray board (r, c) '#'
                let neighbors = [(r', c') | (r', c') <- [(r+1,c),(r-1,c),(r,c+1),(r,c-1)],
                                           0 <= r', r' < m, 0 <= c', c' < n]
                child <- foldM (dfs path') (Trie False next) neighbors
                writeArray board (r, c) ch
                let children' = if not (isWord child) && M.null (trieChildren child)
                      then M.delete ch children
                      else M.insert ch child children
                pure (Trie terminal children')
      _ <- foldM (dfs []) root cells
      reverse <$> readSTRef found
-- END 111

-- BEGIN 101
maxSlidingWindow :: [Int] -> Int -> [Int]
maxSlidingWindow nums k
  | k <= 0 = []
  | otherwise = reverse $ snd $ foldl' step (Q.empty, []) (zip [0..] nums)
  where
    expire i queue = case Q.viewl queue of
      (j, _) Q.:< rest | j <= i - k -> expire i rest
      _ -> queue
    trim x queue = case Q.viewr queue of
      rest Q.:> (_, value) | value < x -> trim x rest
      _ -> queue
    step (queue, answer) (i, x) =
      let queue' = trim x (expire i queue) Q.|> (i, x)
      in case Q.viewl queue' of
        (_, best) Q.:< _ | i >= k - 1 -> (queue', best : answer)
        _ -> (queue', answer)
-- END 101

-- BEGIN 93
-- 辞書は Trie。文字列の全 suffix は tails で共有し、!! や毎回の drop を避ける。
wordBreak :: String -> [String] -> Bool
wordBreak s dictionary = runST $ do
  let n = length s
      root = foldl' (flip trieInsert) emptyTrie dictionary
  dp <- newBools (0, n) False
  writeArray dp 0 True
  let walk !_ _ [] = pure ()
      walk !end node (ch : rest) = case M.lookup ch (trieChildren node) of
        Nothing -> pure ()
        Just child -> do
          when (isWord child) $ writeArray dp (end + 1) True
          walk (end + 1) child rest
  forM_ (zip [0..] (tails s)) $ \(start, suffix) -> do
    reachable <- readArray dp start
    when reachable $ walk start root suffix
  readArray dp n

-- 追い質問: suffix から見た DP。辞書語が終わる位置を Trie で列挙する。
wordEnds :: Int -> String -> Trie -> [Int]
wordEnds !_ [] _ = []
wordEnds !i (ch : rest) node = case M.lookup ch (trieChildren node) of
  Nothing -> []
  Just child -> [i + 1 | isWord child] ++ wordEnds (i + 1) rest child

wordBreakOne :: String -> [String] -> Maybe [String]
wordBreakOne s dictionary = dp A.! 0
  where
    n = length s
    root = foldl' (flip trieInsert) emptyTrie dictionary
    dp = A.listArray (0, n)
      [ if i == n then Just [] else firstJust
          [(take (j - i) suffix :) <$> (dp A.! j) | j <- wordEnds i suffix root]
      | (i, suffix) <- zip [0..] (tails s)]
    firstJust [] = Nothing
    firstJust (Nothing : rest) = firstJust rest
    firstJust (result : _) = result

wordBreakCount :: String -> [String] -> Integer
wordBreakCount s dictionary = dp A.! 0
  where
    n = length s
    root = foldl' (flip trieInsert) emptyTrie dictionary
    dp = A.listArray (0, n)
      [if i == n then 1 else sum [dp A.! j | j <- wordEnds i suffix root]
      | (i, suffix) <- zip [0..] (tails s)]
-- END 93

-- BEGIN 86
rob :: [Int] -> Int
rob = snd . foldl' step (0, 0)
  where
    step (!prev2, !prev1) x =
      let !best = max prev1 (prev2 + x)
      in (prev1, best)
-- END 86

-- BEGIN 95
canPartition :: [Int] -> Bool
canPartition nums
  | odd total = False
  | otherwise = runST $ do
      dp <- newBools (0, target) False
      writeArray dp 0 True
      forM_ nums $ \x -> forM_ [target, target - 1 .. x] $ \s -> do
        reachable <- readArray dp (s - x)
        when reachable $ writeArray dp s True
      readArray dp target
  where
    total = sum nums
    target = total `div` 2
-- END 95

-- BEGIN 94
lengthOfLIS :: [Int] -> Int
lengthOfLIS nums = runST $ do
  smallest <- newInts (0, length nums - 1) 0
  let lowerBound !x !lo !hi
        | lo == hi = pure lo
        | otherwise = do
            let mid = lo + (hi - lo) `div` 2
            value <- readArray smallest mid
            if value < x then lowerBound x (mid + 1) hi else lowerBound x lo mid
      step !size x = do
        i <- lowerBound x 0 size
        writeArray smallest i x
        pure $! if i == size then size + 1 else size
  foldM step 0 nums
-- END 94

-- BEGIN 45
data Tree = Empty | Node !Int Tree Tree
  deriving (Eq, Show)

maxPathSum :: Tree -> Maybe Int
maxPathSum = snd . gain
  where
    gain Empty = (0, Nothing)
    gain (Node value left right) =
      let (!lg, !lb) = gain left
          (!rg, !rb) = gain right
          !l = max 0 lg
          !r = max 0 rg
          !through = value + l + r
          !best = max (fromMaybe through lb) (max through (fromMaybe through rb))
          !up = value + max l r
      in (up, Just best)
-- END 45

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
  _ <- foldM step [] (zip [0..] temperatures)
  getElems answer
-- END 21

-- BEGIN 54
combinationSum :: [Int] -> Int -> [[Int]]
combinationSum candidates target = go target (sort candidates)
  where
    go 0 _ = [[]]
    go _ [] = []
    go remaining choices@(x : rest)
      | x > remaining = []
      | otherwise = map (x :) (go (remaining - x) choices) ++ go remaining rest
-- END 54

-- BEGIN 25
minEatingSpeed :: [Int] -> Int -> Int
minEatingSpeed [] _ = 0
minEatingSpeed piles hours = search 1 (maximum piles)
  where
    feasible speed =
      foldl' (\total p -> total + (toInteger p + toInteger speed - 1) `div` toInteger speed) 0 piles
        <= toInteger hours
    search !lo !hi
      | lo == hi = lo
      | feasible mid = search lo mid
      | otherwise = search (mid + 1) hi
      where mid = lo + (hi - lo) `div` 2
-- END 25

-- BEGIN 79
findRedundantConnection :: [(Int, Int)] -> Maybe (Int, Int)
findRedundantConnection edges = runST $ do
  let n = length edges -- LC の節点ラベルは 1..n
  parent <- newInts (0, n) 0
  size <- newInts (0, n) 1
  forM_ [0 .. n] $ \i -> writeArray parent i i
  let find x = do
        p <- readArray parent x
        if p == x then pure x else do
          root <- find p
          writeArray parent x root -- path compression
          pure root
      loop [] = pure Nothing
      loop ((a, b) : rest) = do
        ra <- find a
        rb <- find b
        if ra == rb then pure (Just (a, b)) else do
          sa <- readArray size ra
          sb <- readArray size rb
          let (small, big) = if sa < sb then (ra, rb) else (rb, ra)
          writeArray parent small big
          writeArray size big (sa + sb) -- union by size
          loop rest
  loop edges
-- END 79

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

-- BEGIN 270
-- Nothing を「内部では切れない = ∞」として使う。浮動小数点は不要。
data CutNode = CutNode
  { leftChild :: Maybe (Integer, CutNode)
  , rightChild :: Maybe (Integer, CutNode)
  } deriving (Show)

minCutLeaves :: Maybe CutNode -> Integer
minCutLeaves Nothing = 0
minCutLeaves (Just root) = fromMaybe 0 (cost root)
  where
    cost (CutNode Nothing Nothing) = Nothing
    cost (CutNode left right) = Just $ foldl' (+) 0
      [maybe w (min w) (cost child) | Just (w, child) <- [left, right]]

data NaryNode = NaryNode [(Integer, NaryNode)]
  deriving (Show)

minCutNary :: NaryNode -> Integer
minCutNary = fromMaybe 0 . cost
  where
    cost (NaryNode []) = Nothing
    cost (NaryNode children) = Just $ foldl' (\total (w, child) ->
      total + maybe w (min w) (cost child)) 0 children

-- 負の重みの解釈 (a): 切った部分木の中の辺も集合に含められる。
minCutTakeNegatives :: NaryNode -> Integer
minCutTakeNegatives root =
  let (negative, extra) = cost root
  in negative + fromMaybe 0 extra
  where
    cost (NaryNode []) = (0, Nothing)
    cost (NaryNode children) =
      let (!negative, !extra) = foldl' step (0, 0) children
      in (negative, Just extra)
    step (!negative, !extra) (w, child) =
      let (!belowNegative, !belowExtra) = cost child
          !negative' = negative + belowNegative + min 0 w
          !extra' = extra + if w < 0 then 0 else maybe w (min w) belowExtra
      in (negative', extra')
-- END 270
