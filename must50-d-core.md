# Must 50 — D. 型の基本(Core 99 ほか)(25 問) — Haskell 版

A〜C の土台。

86 以降は偏りを直すために足した型(DP、スタック、二分探索、union-find、累積和と差分配列、木 DP)。

覚えるのは共通の骨組み([scripts.md](../scripts.md) R1a)と、各問の「言うこと 2 行」だけ。
元の [must50-d-core.md](../must50-d-core.md) の目次にある 25 問と、本文中の補足 260 を Haskell に移したもの。

問題文・Trace・面接スクリプトを引き継ぎ、実装に合わせて説明を調整した。
問題ごとの解答と実行用 main は [Solutions/](Solutions/)、全問の再エクスポートは [Must50DCore.hs](Must50DCore.hs)、
代表例と小さい入力の全探索との照合は [test/Check.hs](test/Check.hs)。

この Haskell 版は元の生成ツールの対象外。

## 使い方

Nix がある環境で、リポジトリ直下から:

```sh
cd hs
nix run path:.
nix run path:.#repl
# GHCi: mergeIntervals [(1,3),(2,6),(8,10)]
# GHCi: wordBreak "leetcode" ["leet","code"]
# GHCi: :quit
nix run path:.#test
```

依存は `base`、`array`、`containers`、`hashtables`。

Nix が GHC と依存を供給する。

GHC 9.10.3 でコンパイルとテストを確認済み。

編集用の `nix develop path:.` と Cabal の使い方は [README.md](README.md) を参照。

- `dict` の平均 O(1) が必要な問題は `hashtables` を使う。`Data.Map` は平衡木で O(log n)。

- 添字アクセスや書き換えは `STUArray`。

  `runST` で局所的な更新を閉じ込めると、呼び出し側からは純粋関数になる。

- heap は下の共通実装、deque / queue は `Data.Sequence`。

  Trie の子は `Map Char Trie` で、英小文字の制約なら分岐数は最大 26。

- 配列は 0 始まり。

  グラフの節点番号は各問題に従う。

  `Int` は 64 bit 環境を前提とし、問 25 の合計と問 270 の重みには `Integer` を使う。

- `!` と `foldl'` は累積値の遅延を溜めないために使う。

  状態を変える API は `IO` か、新しい状態を返す純粋関数で表す。

- 以下のコード片は共通の import・補助関数を共有する。まとめて実行するには `Must50DCore.hs` を読み込む。

<details><summary>共通の import と補助関数(heap・配列)</summary>

```haskell
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
```

heap は leftist heap。

各ノードに rank と要素数を保存し、`heapPush` / `heapPop` は O(log n)、`heapPeek` / `heapSize` は O(1)。

同じ値も別の要素として保持する。

`Down x` を入れると max-heap になる。

</details>

| 問                                                           | 問題                                           | key idea                                                                      |
| ------------------------------------------------------------ | ---------------------------------------------- | ----------------------------------------------------------------------------- |
| [61](#61-merge-intervals-lc56)                               | Merge Intervals                                | Sort by start. Merge into the last interval when they overlap.                |
| [64](#64-meeting-rooms-ii-lc253)                             | Meeting Rooms II                               | Sort by start. A min heap of end times counts the rooms in use.               |
| [68](#68-number-of-islands-lc200)                            | Number of Islands                              | Scan the grid. DFS from each new land cell and mark it.                       |
| [75](#75-course-schedule-lc207)                              | Course Schedule                                | Topological sort with in-degrees. It works if every node gets removed.        |
| [80](#80-word-ladder-lc127)                                  | Word Ladder                                    | BFS over words. Neighbors differ by one letter, found with wildcard patterns. |
| [82](#82-network-delay-time-lc743)                           | Network Delay Time                             | Dijkstra from the source. The answer is the largest shortest distance.        |
| [34](#34-lru-cache-lc146)                                    | LRU Cache                                      | A hash map plus a doubly linked list in use order.                            |
| [5](#5-top-k-frequent-elements-lc347)                        | Top K Frequent Elements                        | Count, then bucket by count and read from the top.                            |
| [52](#52-find-median-from-data-stream-lc295)                 | Find Median From Data Stream                   | Two heaps: a max heap for the low half, a min heap for the high half.         |
| [14](#14-longest-substring-without-repeating-characters-lc3) | Longest Substring Without Repeating Characters | A sliding window with last-seen indexes. Jump the left side past a repeat.    |
| [108](#108-implement-trie-lc208)                             | Implement Trie                                 | Nested children maps with an end-of-word mark.                                |
| [111](#111-word-search-ii-lc212)                             | Word Search II                                 | Put the words in a trie, then DFS the grid while walking the trie.            |
| [101](#101-sliding-window-maximum-lc239)                     | Sliding Window Maximum                         | A deque of indexes with decreasing values. The front is the max.              |
| [93](#93-word-break-lc139)                                   | Word Break                                     | dp at i is true if some word ends at i and dp at its start is true.           |
| [86](#86-house-robber-lc198) | House Robber | The best up to a house is the max of skipping it, or robbing it plus the best from two houses back. |
| [95](#95-partition-equal-subset-sum-lc416) | Partition Equal Subset Sum | 0/1 knapsack: can a subset make half the total? Go through the sums from high to low so each number is used once. |
| [94](#94-longest-increasing-subsequence-lc300) | Longest Increasing Subsequence | Keep the smallest tail for each length. Binary search where each number goes. |
| [45](#45-binary-tree-maximum-path-sum-lc124) | Binary Tree Maximum Path Sum | DFS returns the best one-sided path. At each node, update the answer with left plus node plus right. |
| [21](#21-daily-temperatures-lc739) | Daily Temperatures | A stack of indexes with decreasing temperatures. A warmer day pops them and fills in their answers. |
| [54](#54-combination-sum-lc39) | Combination Sum | Backtrack over the remaining candidates so each combination is built once. Keep the same candidate list to reuse a number. |
| [25](#25-koko-eating-bananas-lc875) | Koko Eating Bananas | Binary search the speed. The hours needed only go down as the speed goes up. |
| [79](#79-redundant-connection-lc684) | Redundant Connection | Union find over the edges. The first edge whose two ends already share a root makes the cycle. |
| [267](#267-subarray-sum-equals-k-lc560) | Subarray Sum Equals K | Two Sum on prefix sums. Count earlier prefixes equal to the current prefix minus k. |
| [268](#268-increment-submatrices-by-one-実出題-lc2536) | Increment Submatrices by One (実出題) | A 2D difference array: four corner marks per rectangle, then row and column prefix sums. |
| [270](#270-minimum-cost-to-cut-off-all-leaves-実出題) | Minimum Cost to Cut Off All Leaves (実出題) | For each child, cut the edge or cut inside the child, whichever is cheaper. A leaf costs infinity. |

本文中の補足: [260. Status Message Deduplication](#260-status-message-deduplication-実出題)。

---

## 61. Merge Intervals [(LC56)](https://leetcode.com/problems/merge-intervals/)

> **Medium** ・ ソートして重なりを結合

**言うこと(暗記はこの 2 行)**

> The key idea is: Sort by start. Merge into the last interval when they overlap.  
> Time is O of n log n.

### Problem

重なる interval を全て merge。

### Constraints

- `1 <= n <= 10^4`, `start <= end`

- 入力は未ソート。**端が接する(end == 次の start)も結合**

- start でソート → 前の end と比較して結合 O(n log n)

### Brute force

- 全ペア比較: O(n²)。

### Optimized approach + key idea

- **Key idea**: start でソート。直前の end と重なれば end を伸ばす、else 新規。

### 個別ファイルとサンプル入力

[Solutions/MergeIntervals.hs](Solutions/MergeIntervals.hs) に解答本体と実行用 `main` がある。

```sh
nix run path:.#p61
```

```text
入力: mergeIntervals [(1,3),(2,6),(8,10)]
期待する出力: [(1,6),(8,10)]
```

### Haskell code

```haskell
mergeIntervals :: [(Int, Int)] -> [(Int, Int)]
mergeIntervals = reverse . foldl' step [] . sort
  where
    step ((a, b) : rest) (s, e)
      | s <= b = (a, max b e) : rest
    step acc interval = interval : acc
```

### Haskell の読み方

`foldl'` の蓄積リストは逆順で、先頭が「直前の interval」。

`(:)` で O(1) 追加し、最後に `reverse` する。

下の Trace は読みやすい結果順で表す。

### Trace(値の動き)

入力: `intervals = [[1,3],[2,6],[8,10]]`(sort 後も同じ順)

| step | interval | res                        | 動作                                   |
| ---- | -------- | -------------------------- | -------------------------------------- |
| 1    | [1,3]    | `[] → [[1,3]]`             | res が空 → そのまま追加                |
| 2    | [2,6]    | `[[1,3]] → [[1,6]]`        | 2 <= 3 で重なり → end を max(3,6)=6 に |
| 3    | [8,10]   | `[[1,6]] → [[1,6],[8,10]]` | 8 > 6 で重ならない → 追加              |

返り値: `[[1,6],[8,10]]`

### Complexity

- Time: O(n log n)

- Space: O(n)

### Edge cases

- 1 個

- 全部重なる → 1 個

### Interview script

<details><summary>全文(読むだけ。暗記しない)</summary>

This problem gives me a list of intervals.  
I need to merge all intervals that overlap.

Let me confirm a few things first.  
Do touching intervals merge, when one end equals the next start? Yes.  
Is the input unsorted? Yes.

The brute force is to compare all pairs of intervals.  
That is O of n squared time.  
That is too slow.

We can do better.  
The key idea is to sort by start, then merge each interval into
the last result when they overlap.

Here is my plan.  
I sort the intervals by start.  
I fold over the sorted intervals and keep the result in reverse order, so the last interval is at the
head.
I scan the intervals from left to right.  
For each interval, I check if its start is at most the end of the last result.  
If yes, I update that end with the larger end. If no, I add the interval to the result.  
At the end, I reverse the result and return it.

Let me trace a small example.  
The input is [[1, 3], [2, 6], [8, 10]].  
[2, 6] starts at 2, at most 3, so the last result grows to [1, 6].  
[8, 10] starts after 6, so I add it.  
That matches.

Time is O of n log n, because the sort is the largest cost.  
Space is O of n, because the result list can hold all intervals.

For edge cases, one interval returns as is, and when all intervals overlap, they merge into one.

</details>

### Follow-up

- Q: What if intervals arrive one at a time?

  → A: I keep a sorted list and merge on each insert, like Insert Interval.

- Q: What if the list is huge and does not fit in memory?

  → A: I sort chunks on disk, merge the sorted chunks like merge sort, then do one merge scan.

- Q: What if the input is already sorted by start? → A: Then I skip the sort and merge in one O of n
  scan.

---

## 64. Meeting Rooms II [(LC253)](https://leetcode.com/problems/meeting-rooms-ii/)

> **Medium** ・ 開始/終了の min-heap or 差分

**言うこと(暗記はこの 2 行)**

> The key idea is: Sort by start. A min heap of end times counts the rooms in use.  
> Time is O of n log n.

### Problem

全会議に必要な最小部屋数。

### Constraints

- (LC premium: 典型制約 `1 <= n <= 10^4`)

- 必要な部屋数 = 同時に進行中の会議の最大数

- 開始/終了を別ソートして2ポインタ、or min-heap で end 管理

### Brute force

- 各時刻をスキャン: O(n²)。

### Optimized approach + key idea

- **Key idea**: min-heap で各部屋の終了時刻を管理。または start/end をソートする 2 ポインタ (sweep)

### 個別ファイルとサンプル入力

[Solutions/MeetingRoomsII.hs](Solutions/MeetingRoomsII.hs) に解答本体と実行用 `main` がある。

```sh
nix run path:.#p64
```

```text
入力: minMeetingRooms [(0,30),(5,10),(15,20)]
期待する出力: 2
```

### Haskell code

```haskell
minMeetingRooms :: [(Int, Int)] -> Int
minMeetingRooms = heapSize . foldl' step HEmpty . sort
  where
    step heap (s, e) =
      case heapPop heap of
        Just (end, rest) | end <= s -> heapPush e rest
        _ -> heapPush e heap
```

### Haskell の読み方

共通の `Heap Int` に各部屋の最後の終了時刻を持つ。

`heapPop` で再利用候補を見て、使えなければ元の heap に追加する。

### 解説(なぜ動くか)

- **不変条件**: heap は「確保した各部屋の最後の終了時刻」を 1 個ずつ持つ。

  空いた部屋も残るので、heap サイズ = これまで確保した部屋数。

- start 順に処理するとき、`heap[0] (最小の end) <= start` なら「一番早く空く部屋」を再利用できる → pop
  してから push(部屋数は変わらない)。

- 再利用できないときは push だけ → 部屋が 1 つ増える。

  heap サイズは減らないので、最後のサイズ = 同時進行の最大数 = 必要部屋数。

- つまずき: pop は会議 1 件につき最大 1 回でよい(1 会議が空けるのは 1 部屋だけ)。

- `end == start` は再利用可(back to back は同じ部屋)。

- 別解: start 配列と end 配列を別々にソートして 2 ポインタで「進行中」を数えても同じ答え。

### Trace(値の動き)

入力: `intervals = [[0,30],[5,10],[15,20]]`

| step | meeting | heap (end times)    | 動作                               |
| ---- | ------- | ------------------- | ---------------------------------- |
| 1    | [0,30]  | `[] → [30]`         | 空き部屋なし → push 30             |
| 2    | [5,10]  | `[30] → [10,30]`    | min 30 > 5 → 再利用不可、部屋追加  |
| 3    | [15,20] | `[10,30] → [20,30]` | min 10 <= 15 → pop 10 して push 20 |

返り値: `len(heap) = 2`

### Complexity

- Time: O(n log n)

- Space: O(n)

### Edge cases

- 1 会議 → 1

- 全部重なる → n

### Interview script

<details><summary>全文(読むだけ。暗記しない)</summary>

This problem gives me meeting intervals.  
I need the minimum number of rooms for all meetings.

Let me confirm a few things first.  
Can I use a room again when its meeting ends? Yes.  
Does an end equal to the next start free the room? Yes.

The brute force is to count the running meetings at every time point.  
That is O of n squared time.  
That is too slow.

We can do better.  
The key idea is a min heap of end times, one per open room.

Here is my plan.  
I sort the meetings by start.  
I keep a heap called ends.  
I scan the meetings from left to right.  
For each meeting, I check if the smallest end is at most its start.  
If yes, I pop that room, because it is free. If no, the meeting needs one more room.  
I push the meeting end into the heap.  
At the end, I return the heap size.

Let me trace a small example.  
The input is [[0, 30], [5, 10], [15, 20]].  
[5, 10] starts before 30, so it needs a second room.  
[15, 20] starts after 10, so it takes that free room, and the heap size stays 2.  
That matches.

Time is O of n log n, because of the sort and the heap pushes.  
Space is O of n, because the heap can hold all meetings.

For edge cases, one meeting needs one room, and when all meetings overlap, each needs its own room.

</details>

### Follow-up

- Q: How would you also give each meeting a room number?

  → A: I store the room id with the end time in the heap, and I use the room that frees first again.

- Q: Can you do it without a heap?

  → A: Yes.

  I sort the starts and the ends, then scan with two pointers and count the running meetings.

- Q: How do I check if one room is enough?

  → A: That is Meeting Rooms.

  I sort by start and check the pairs next to each other.

---

## 68. Number of Islands [(LC200)](https://leetcode.com/problems/number-of-islands/)

> **Medium** ・ Grid DFS: 新しい陸を見つけたら stack で島ごと沈めて count++

**言うこと(暗記はこの 2 行)**

> The key idea is: Scan the grid. DFS from each new land cell and mark it.  
> Time is O of m n.

### Problem

0/1 グリッドの連結した 1(陸)の塊 = 島の数を返す。

### Constraints

- `1 <= m, n <= 300`, セルは文字 `'0'` / `'1'`

- 入力は変更せず、`runST` 内の陸フラグを False にする

- m*n <= 9*10^4 → O(m\*n) の DFS flood fill

### Brute force

- 各セルから無計画に探索すると同じ島を何度も数える。「一度数えた陸は二度と数えない」仕組みが必要。

### Optimized approach + key idea

- **Key idea**: 新しい陸を見つけたら count++。明示的な stack で島全体を探索する。

- stack に積む時点で訪問済みにする。同じセルを二度積まない。

- 隣接は上下左右のみ。再帰呼び出しで木の深さを持つ代わりに、未処理セルをリストに持つ。

### 個別ファイルとサンプル入力

[Solutions/NumberOfIslands.hs](Solutions/NumberOfIslands.hs) に解答本体と実行用 `main` がある。

```sh
nix run path:.#p68
```

```text
入力: numIslands ["110","001"]
期待する出力: 2
```

### Haskell code

```haskell
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
```

### Haskell の読み方

`runST` の中だけ `STUArray` を更新する。

明示的なリストの stack で DFS し、積む時点で陸を消す。

外から見ると入力を変更しない純粋関数。

### 解説(なぜ動くか)

- stack 内のセルは既に訪問済み。同じ島から別の DFS を始めることはない。

- セルは高々 1 回 stack に入り、各セルの隣は 4 個なので O(mn)。

- `sink` は明示的な stack を受け取る末尾再帰。陸フラグは `STUArray` で O(1) アクセスする。

- 元の grid は不変で、呼び出した後も使える。

### Trace(値の動き)

入力(2x3 grid):

```
1 1 0
0 0 1
```

| step | cell      | count / stack                     | 動作                        |
| ---- | --------- | --------------------------------- | --------------------------- |
| 1    | (0,0)     | count 0 → 1, stack `[] → [(0,0)]` | 新しい陸 → 沈めて push      |
| 2    | pop (0,0) | stack `[(0,0)] → [(0,1)]`         | 隣 (0,1) が陸 → 沈めて push |
| 3    | pop (0,1) | stack `[(0,1)] → []`              | 陸の隣なし → DFS 終了       |
| 4    | (1,2)     | count 1 → 2, stack `[] → [(1,2)]` | 新しい陸 → 沈めて push      |
| 5    | pop (1,2) | stack `→ []`                      | 陸の隣なし                  |

返り値: `2`

### Complexity

- Time: O(mn)

- Space: O(mn)(陸フラグの配列と明示的な stack)

### Edge cases

- 全部 '0' → 0

- 全部 '1' → 1

- 1 行 / 1 列だけのグリッド

### Interview script

<details><summary>全文(読むだけ。暗記しない)</summary>

This problem gives me a grid of land and water.  
I need the number of islands, the groups of connected land cells.

Let me confirm a few things first.  
Are cells connected up, down, left, and right only, no diagonals? Yes.  
I keep a local mutable array of land flags, so the input stays available to the caller.

The brute force is to search from every cell with no visited mark, so I count the same island many
times.
That is O of m n times m n time.  
That is too slow.

We can do better.  
The key idea is DFS flood fill: when I see new land, I count one island and sink the whole island with
a stack.

Here is my plan.  
I visit each cell one by one.  
When I see land, I add one to the count.  
I mark the cell as visited by updating it to water, and I push it into the stack.  
For each cell I pop, I check its four neighbors.  
If a neighbor is land, I mark it first, then I push it.  
Marking at push time stops the same cell from entering the stack twice.  
At the end, I return the count.

Let me trace a small example.  
The input is a 2 by 3 grid with a land pair at the top left and one land cell at the bottom right.  
The first land cell sinks the pair, so the count is 1.  
The lone cell at the bottom right makes the count 2.  
That matches.

Time is O of m times n, because each cell enters the stack at most once.  
Space is O of m times n for the land flags and the explicit stack.

For edge cases, all water returns zero, and all land returns one.

</details>

### Follow-up

- Q: What if I cannot update the grid? → A: I keep a visited set of cells and check it before each push.

## 260. Status Message Deduplication (実出題)

> **Easy** ・ message → 最後に表示した時刻の map(LC359 Logger Rate Limiter 系)。隠した分は窓をリセットしない
> 出典: 独立 10 件以上(2024-08〜2026-09、L3〜Senior)/ 電話スクリーン 6 件(全部 L4)/ HI の LC359 ページに 2026 年の Google 報告 5 件 / LC 6775982 (2025-05)、LC 8500920 (2026-09、追い質問 1)、PracHub 2026-07 と 2026-08 L4 onsite(追い質問 2)

**言うこと(暗記はこの 2 行)**

> The key idea is: Map each message to the last time it was shown. (message: lasttime)
> Time is O of one per message. Space is the number of distinct messages.

### Problem

> [Q] "Received: 10 solar panel activated / 11 low battery warning / 12 tire one: low air pressure / 13 solar panel activated / 14 low battery warning / 21 solar panel activated / 35 solar panel activated. Shown: 10, 11, 12, 21, 35." (LC 6775982)
> [Q] "a message reappears only when strictly more than ten seconds have passed since its last displayed occurrence ... ensure hidden repeats do not reset the interval." (PracHub, 2026-07)

車のステータスメッセージが `(timestamp, message)` で流れてくる。

`shouldPrint deduper timestamp message :: IO Bool` を実装し、
**同じ文面を最後に表示してから 10 秒より長く**経っていれば表示(`True`)、そうでなければ隠す(`False`)。

隠した出現は「最後に表示した時刻」を動かさない。

**仮定**

- 境界は「厳密に 10 秒より長い」(`gap > 10` で表示、`gap == 10` は隠す)。

  `>=` 版は `newMessageDeduper 10 False` で切り替える

- timestamp は非減少で届く(順不同は追い質問 2)。同じ文面は完全一致(大文字小文字・空白は区別)

- 隠した出現は窓をリセットしない: 13 で `solar` を隠しても 21 は 10 から数えて 11 秒なので表示

- メッセージの種類数 M は有限だが、ストリームは終わらない(メモリの話は追い質問 3)

聞くこと: 境界は `>10` か `>=10` か / 隠した出現は窓をリセットするか(報告は「しない」)/ timestamp
は順に届くか / 文面の正規化(大文字小文字・前後の空白)は要るか / 返り値は bool か、
表示する文面のリストか。

### Constraints

- 呼び出し回数 `n <= 10^6`、`0 <= timestamp <= 10^9`(非減少)、文面の長さ `<= 100`

- **隠した出現で `last_shown` を更新しない**のが要件の芯。更新すると連打中のメッセージが永久に出なくなる

- 1 回の判定 O(1)。空間 O(M)(M = 文面の種類数)

### Brute force

- 表示したメッセージを全部リストに持ち、毎回「同じ文面で 10 秒以内に表示したもの」を走査: 1 回
  O(表示数)。

  10^6 件で効く。

### Optimized approach + key idea

- **Key idea**: `message → 最後に表示した時刻` の map を 1 つ持つ。

  判定は「map に無い、または `now - last > 10`」。

  表示したときだけ map を書き換える。

- 追い質問 1(両方を抑止)は規則が逆転して**到着時に決められない**ので、`now > ts + 10`
  まで保留するキュー(時刻順)と `message → 最新エントリ` の map で「隣が 10 秒以内なら両方に印」を付け、
  確定したものだけ出す。

### 個別ファイルとサンプル入力

[Solutions/StatusMessageDeduplication.hs](Solutions/StatusMessageDeduplication.hs) に解答本体と実行用 `main` がある。

```sh
nix run path:.#p260
```

```text
入力: window=10, strict=True, message="solar", timestamps=[10,13,20,21,31,35]
期待する出力: [True,False,False,True,False,True]
```

### Haskell code

```haskell
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
```

### Haskell の読み方

`newMessageDeduper 10 True` は gap > 10、`False` は gap >= 10。

判定 API は `IO`。

追い質問の `SuppressBothDeduper` は確定前のメッセージを queue に保留し、`IORef Bool`
に両方を抑止する印を付ける。

### 解説(なぜ動くか)

- `lastShown` は最後に表示した時刻だけを持つ。

  隠した出現で更新すると、連打されるメッセージが永遠に出なくなる。

- 境界 `gap == 10` を表示するかは `strict` で切り替える。timestamp が非減少という前提で動く。

- 文面の hash と比較に O(L)。文面長の上限を定数と見れば基本 API は平均 O(1)、空間 O(M)。

- 両方を隠す追い質問では、未来の重複が分かるまで出力できない。

  `now > timestamp + window` で初めて確定する。

- 最新エントリを map に持ち、近い到着の前後に同じ可変フラグを通じて印を付ける。0,10,20
  の連鎖も全件隠す。

- 解放時は、そのエントリが map 内の最新なら map から削除する。

  古いエントリの解放で新しいエントリを消さない。

- 各エントリは queue に 1 回入り 1 回出るので、追い質問も 1 件あたり償却平均 O(1)(文面長固定)。

  最後は `suppressFlush` で残りを確定する。

### Trace(値の動き)

`newMessageDeduper 10 True` に同じ文面を時刻 10,13,20,21,31,35 で渡すと、返り値は
`True,False,False,True,False,True`。

13 と 20 を隠しても最後の表示は 10 のままなので、21 は表示する。

### Complexity

- 基本: 平均 O(L) / 件、空間 O(全ての異なる文面の合計長)。L を固定すると O(1) / 件、O(M) 空間。

- 両方抑止: 同じ時間計算量を償却で満たす。空間は保留中の到着と文面の量に比例する。

### Follow-up

- 両方を抑止: `suppressAdd` は確定した出力だけを返す。`suppressFlush` はストリーム終端で呼ぶ。

- 順不同: 許容する遅れを確認し、watermark と時刻順 buffer で確定を遅らせる。

  遅れが無制限なら有限時間で確定できない。

- 基本版のメモリ上限: 有効期限付き queue で古い map エントリを消す。

  map の時刻と一致する場合だけ消し、更新済みの値を誤って削除しない。

---

## 75. Course Schedule [(LC207)](https://leetcode.com/problems/course-schedule/)

> **Medium** ・ Topo sort: indegree 0 から消化、全消化で可能

**言うこと(暗記はこの 2 行)**

> The key idea is: Topological sort with in-degrees. It works if every node gets removed.  
> Time is O of V plus E.

### Problem

前提条件グラフが非巡回か (全部履修可能か)。

### Constraints

- `1 <= numCourses <= 2000`, `0 <= prereqs <= 5000`

- 履修可能 ⇔ 依存グラフに閉路なし

- Kahn: indegree 0 から消化。全部消化できれば true。O(V+E)

### Brute force

- 各ノードからサイクル探索を毎回: 重複。

### Optimized approach + key idea

- **Key idea**: Topological sort (Kahn)。入次数 0 を順に消す。全部消せれば DAG。

### 個別ファイルとサンプル入力

[Solutions/CourseSchedule.hs](Solutions/CourseSchedule.hs) に解答本体と実行用 `main` がある。

```sh
nix run path:.#p75
```

```text
入力: canFinish 3 [(2,1),(1,0)]
期待する出力: True
```

### Haskell code

```haskell
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
```

### Haskell の読み方

`(course, prerequisite)` の順。

隣接リストは `Array`、入次数は `STUArray`、queue は `Data.Sequence`。

`viewl` で先頭を取り、`(|>)` で末尾へ追加する。

### 解説(なぜ動くか)

- **Kahn 法の不変条件**: queue には「未処理の前提が 0 個の課目」だけが入る。

- 課目を pop するたび、その課目を前提とする課目の in-degree を subtract one → 0
  になった瞬間に受講可能になり push。

- 閉路上の課目は in-degree が決して 0 にならず、永遠に queue に入らない → `seen < numCourses`
  で閉路を検出。

- つまずき: 辺の向きは pre → course(前提から後続へ)。逆に張ると in-degree の意味が壊れる。

- DFS の 3 色(未訪問/探索中/完了)でも閉路判定できるが、Kahn の方が口頭で説明しやすい。

### Trace(値の動き)

入力: `numCourses = 3`, `prerequisites = [[1,0],[2,1]]`(0 → 1 → 2 の鎖)

| step | pop | indeg / queue / seen                                 | 動作                         |
| ---- | --- | ---------------------------------------------------- | ---------------------------- |
| 0    | —   | indeg `[0,1,1]`, queue `[0]`, seen 0                 | in-degree 0 の課目 0 を push |
| 1    | 0   | indeg `[0,1,1] → [0,0,1]`, queue `→ [1]`, seen 0 → 1 | 課目 1 が 0 に → push        |
| 2    | 1   | indeg `→ [0,0,0]`, queue `→ [2]`, seen 1 → 2         | 課目 2 が 0 に → push        |
| 3    | 2   | queue `→ []`, seen 2 → 3                             | 後続なし                     |

返り値: `seen 3 == 3 → True`

### Complexity

- Time: O(V + E)

- Space: O(V + E)

### Edge cases

- prerequisites 空 → True

- 自己ループ → False

### Interview script

<details><summary>全文(読むだけ。暗記しない)</summary>

This problem gives me courses and prerequisite pairs.  
I need to check if I can finish all courses, so the graph has no cycle.

Let me confirm a few things first.  
Does a pair mean one course comes before the other? Yes.  
Can a course depend on itself? Maybe, and that is a cycle.

The brute force is to run a cycle search from every node.  
That is O of V times E time.  
That is too slow.

We can do better.  
The key idea is topological sort with in-degree counts: a course with in-degree zero has no
prerequisite left, so I can take it.

Here is my plan.  
I build the graph and an in-degree count for each course.  
I push every course with in-degree zero into the queue.  
I pop a course, count it as taken, and subtract one from the in-degree of each neighbor.  
If a neighbor hits in-degree zero, I push it.  
I repeat until the queue is empty.  
At the end, I return true if the taken count equals the course count.

Let me trace a small example.  
The input is 3 courses with prerequisites [[1, 0], [2, 1]], a chain.  
Course 0 starts at in-degree zero, then 1 and 2 follow.  
All 3 are taken, so the answer is true.  
That matches.

Time is O of V plus E, because I see each course and each edge once.  
Space is O of V plus E, because of the graph and the queue.

For edge cases, no prerequisites returns true, and a self-loop is a cycle, so false.

</details>

### Follow-up

- Q: How would you return a valid order?

  → A: That is Course Schedule II.

  The pop order of the queue is the answer.

- Q: Can you check the cycle with DFS?

  → A: Yes, with three states per node; an edge back to a node on the current path means a cycle.

- Q: What if the graph does not fit in memory?

  → A: I keep the in-degree counts in memory and stream the edges from disk, one scan per level.

---

## 80. Word Ladder [(LC127)](https://leetcode.com/problems/word-ladder/)

> **Hard** ・ BFS: 1文字違いを辺として最短手数

**言うこと(暗記はこの 2 行)**

> The key idea is: BFS over words. Neighbors differ by one letter, found with wildcard patterns.  
> Time is O of n times L squared.

### Problem

`beginWord` から `endWord` まで 1 文字ずつ変える最短変換列の長さ。

### Constraints

- `1 <= len <= 10`, `1 <= wordList.length <= 5000`, 全単語同じ長さ・小文字

- beginWord はリストに無くてもよい。**endWord が無ければ 0**。数は両端を含む

- パターン map(1文字を \* に)で隣接生成 + BFS で最短

### Brute force

- 全変換列を DFS: 指数的。

### Optimized approach + key idea

- **Key idea**: BFS。隣接 = 1 文字違いの辞書語。パターン `h*t` で隣接を高速生成。

### 個別ファイルとサンプル入力

[Solutions/WordLadder.hs](Solutions/WordLadder.hs) に解答本体と実行用 `main` がある。

```sh
nix run path:.#p80
```

```text
入力: ladderLength "hit" "cog" ["hot","dot","dog","lot","log","cog"]
期待する出力: 5
```

### Haskell code

```haskell
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
```

### Haskell の読み方

`hashtables` の bucket と visited を `runST` に閉じ込める。

一度展開した pattern は削除し、同じ隣接リストを再走査しない。

### 解説(なぜ動くか)

- 単語をノード、1 文字違いを辺とみなす**暗黙のグラフ**の BFS = 最短変換列。

  辺は作らず、その場で隣接を生成する。

- **パターン map が肝**: 1 文字を `*` に置き換えた `h*t` のようなキーで単語を束ねると、
  同じキーの単語同士がちょうど 1 文字違い。

  隣接候補を辞書全走査せず O(L) 個のキー参照で得られる。

- **不変条件**: BFS なので pop した時点の steps はその単語への最短手数。

  最初に endWord を pop した瞬間の steps が答え。

- visited は push 時に追加 → 同じ単語の二重 push を防ぐ。

- つまずき: endWord がリストに無ければ即 0 / 数え方は両端込み(hit→cog は 5)。

- 高速化: 両端 BFS(常に小さい側のフロンティアを広げる)で探索空間が激減。

### Trace(値の動き)

入力: `beginWord = "hit"`, `endWord = "cog"`, `wordList = [hot, dot, dog, cog]`

| step | pop (word, steps) | queue / visited                     | 動作                       |
| ---- | ----------------- | ----------------------------------- | -------------------------- |
| 0    | —                 | queue `[(hit,1)]`, visited `{hit}`  | 開始                       |
| 1    | (hit, 1)          | queue `→ [(hot,2)]`, visited `+hot` | パターン h\*t → hot        |
| 2    | (hot, 2)          | queue `→ [(dot,3)]`, visited `+dot` | \*ot → dot(hot は visited) |
| 3    | (dot, 3)          | queue `→ [(dog,4)]`, visited `+dog` | do\* → dog                 |
| 4    | (dog, 4)          | queue `→ [(cog,5)]`, visited `+cog` | \*og → cog                 |
| 5    | (cog, 5)          | —                                   | endWord に到達 → 5 を返す  |

返り値: `5`

### Complexity

- Time: O(N \* L²)

- Space: O(N \* L²)

### Edge cases

- endWord が辞書にない → 0

- beginWord == endWord

### Interview script

<details><summary>全文(読むだけ。暗記しない)</summary>

This problem gives me a begin word, an end word, and a word list.  
I change one letter at a time, and every step needs to be a word in the list.  
I need the length of the shortest change chain, or zero if there is none.

Let me confirm a few things first.  
Does the count have both end words in it? Yes.  
Does every middle word need to be in the list? Yes.

The brute force is to try all change chains with DFS.  
That is O of two to the n time.  
That is too slow.

We can do better.  
The key idea is BFS for the shortest path, plus a pattern map: I swap one letter for a star, and words
with the same pattern are different in only one letter.

Here is my plan.  
First, I check the end word is in the list; if not, I return zero.  
I build a map from each pattern to its words.  
I push the begin word into the queue with step one, and I keep a visited set.  
For each word I pop, I build its patterns and get the neighbors from the map.  
I skip visited words and push the neighbors into the queue with step plus one.  
When I pop the end word, I return its step count.

Let me trace a small example.  
The input is hit to cog with the list [hot, dot, dog, cog].  
The path is hit, hot, dot, dog, cog.  
That is 5 words, so the answer is 5.  
That matches.

Time is O of N times L squared, because each of the N words builds L patterns of length L.  
Space is O of N times L squared, because of the pattern map.

For edge cases, an end word not in the list returns zero, and a begin word equal to the end word
returns one.

</details>

### Follow-up

- Q: Can you speed this up?

  → A: The bottleneck is the one-sided search space.

  I can fix it with BFS from both ends, always growing the smaller side.

- Q: How do I return the path itself?

  → A: I store each word's previous word in a map and walk back from the end word.

- Q: What if the word list is huge?

  → A: The bottleneck is the pattern map size.

  I can fix it with on-the-fly neighbors: for each word, I try all twenty-six letters at each position.

---

## 82. Network Delay Time [(LC743)](https://leetcode.com/problems/network-delay-time/)

> **Medium** ・ Dijkstra で最大到達時間

**言うこと(暗記はこの 2 行)**

> The key idea is: Dijkstra from the source. The answer is the largest shortest distance.  
> Time is O of E log V.

### Problem

重み付き有向グラフで、ノード `k` から全ノードに届く最小時間。届かないなら -1。

### Constraints

- `1 <= n <= 100`, `1 <= times.length <= 6000`, `0 <= w <= 100`

- **届かないノードがあれば -1**。答えは全ノード到達時刻の最大

- 重み非負 → Dijkstra O(E log V)

### Brute force

- Bellman-Ford: O(V \* E)。

### Optimized approach + key idea

- **Key idea**: Dijkstra。全ノードへの最短時間の最大値が答え。

### 個別ファイルとサンプル入力

[Solutions/NetworkDelayTime.hs](Solutions/NetworkDelayTime.hs) に解答本体と実行用 `main` がある。

```sh
nix run path:.#p82
```

```text
入力: networkDelayTime [(2,1,1),(2,3,1),(3,4,1)] 4 2
期待する出力: 2
```

### Haskell code

```haskell
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
```

### Haskell の読み方

共通の min-heap に `(距離, 節点)` を置く。

`distances` は暫定最短距離。

pop した距離が古ければ捨て、より短い距離だけを push する。

### 解説(なぜ動くか)

- `distances[v]` は v への暫定最短距離。始点は 0、それ以外は `maxBound` (= 到達不能)。

- heap から出た時刻が現在の暫定値と一致すれば、その節点の最短距離を確定できる。

  非負の辺だけなので、後からもっと短くはならない。

- 更新前に積まれた古い候補は `time /= best` で飛ばす。各辺の relax で短くなる場合だけ push する。

- 最後に `maxBound` が残れば -1。残らなければ全距離の最大が答え。

- 負の辺があるなら Dijkstra の前提が壊れる。Bellman-Ford などへ切り替える。

### Trace(値の動き)

入力: `networkDelayTime [(2,1,1),(2,3,1),(3,4,1)] 4 2`。∞ は `maxBound`。

| step | pop (time,node) | distances[1..4] | heap の候補 | 動作 |
| --- | --- | --- | --- | --- |
| 0 | — | [∞,0,∞,∞] | [(0,2)] | 始点だけ 0 |
| 1 | (0,2) | [1,0,1,∞] | [(1,1),(1,3)] | 1 と 3 を relax |
| 2 | (1,1) | [1,0,1,∞] | [(1,3)] | 出辺なし |
| 3 | (1,3) | [1,0,1,2] | [(2,4)] | 4 を relax |
| 4 | (2,4) | [1,0,1,2] | [] | 全節点へ到達 |

返り値: `2`。heap 列は候補の集合を距離順で示す。

### Complexity

- Time: O((V + E) log V)(単純有向グラフ。初期化 O(V) も含む)

- Space: O(V + E)(隣接リスト、距離、heap。古い heap エントリも含む)

### Edge cases

- 到達不能ノード → -1

- n = 1 → 0

### Interview script

<details><summary>全文(読むだけ。暗記しない)</summary>

This problem gives me a directed graph with nonnegative edge weights and a source.  
I need the largest shortest-path distance, or minus one if a node is unreachable.

The brute force uses Bellman-Ford in O of V times E time.

The key idea is Dijkstra.  
I build adjacency lists and initialize every distance to infinity except the source at zero.  
I keep a min heap of distance and node pairs.  
When I pop a pair, I skip it if its distance is stale.  
Otherwise, I relax each outgoing edge and push only improved distances.  
At the end, an infinite distance means minus one; otherwise I return the largest distance.

For the edges two to one, two to three, and three to four, all of weight one, the final distances are
one, zero, one, two.

The answer is two.

Time is O of V plus E times log V, including initialization.  
Space is O of V plus E for the graph, distances, and heap.

A single node takes zero time. Negative weights require a different algorithm.

</details>

### Follow-up

- Q: What if some weights are negative?

  → A: Dijkstra breaks, because a popped node is no longer final.

  I switch to Bellman-Ford.

- Q: Can you cut the duplicate heap items?

  → A: This implementation already pushes only improvements.

  Old entries can still remain; skipping stale entries is enough, or I can use a heap with decrease-key.

- Q: What if the graph is undirected?

  → A: I add each edge in both directions; Dijkstra itself does not change.

---

## 34. LRU Cache [(LC146)](https://leetcode.com/problems/lru-cache/)

> **Medium** ・ hash + 双方向リンクリスト

**言うこと(暗記はこの 2 行)**

> The key idea is: A hash map plus a doubly linked list in use order.  
> Time is O of one per call.

### Problem

LRU キャッシュを get / put 共に O(1) で実装。

### Constraints

- `1 <= capacity <= 3000`, op は最大 2 \* 10^5 回

- **get / put とも平均 O(1) が必須**。get もそのキーを「使用」扱いに

- `hashtables` の hash table + `IORef` の双方向連結リスト

### Brute force

- HashMap だけだと最古検出が O(n)

### Optimized approach + key idea

- **Key idea**: HashMap + doubly linked list。lookup は O(1)、move/evict も O(1)。頭が最新、尾が最古。

### 個別ファイルとサンプル入力

[Solutions/LRUCache.hs](Solutions/LRUCache.hs) に解答本体と実行用 `main` がある。

```sh
nix run path:.#p34
```

```text
入力: capacity=2; put(1,1); put(2,2); get(1); put(3,3); get(2)
期待する出力: [1,-1]
```

### Haskell code

```haskell
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
```

### Haskell の読み方

`newLRUCache` / `lruGet` / `lruPut` は `IO`。

hash table からノードへ直接飛び、前後の `IORef` を付け替える。

`Data.Map` へ置き換えると O(log capacity) になり、元の要件を満たさない。

### 解説(なぜ動くか)

- リストは使用の新しさ順。`cacheHead` が最新、`cacheTail` が最古。hash table は key → 同じノードを指す。

- `detach` で前後をつなぎ直し、`addFront` で先頭へ戻す。前後の参照があるためどちらも O(1)。

- head/tail に `Maybe CacheNode` を使い、`Nothing` を端として扱う。

  1 個だけのリストの削除では両端を更新する。

- get の成功、既存 key の put はどちらも使用順を更新する。miss は使用順を変えない。

- 新規 key だけがサイズを増やす。満杯なら tail を外して table からも消す。容量 0 は何も保存しない。

- 外から見える状態を更新するため `IO`。

  `IORef` の参照先を変えれば、table が指すノードにも同じ変更が見える。

### Trace(値の動き)

入力: capacity = 2, 操作列 `put(1,1), put(2,2), get(1), put(3,3)`

| step | 操作     | list (head→tail) | map の key      | 動作                        |
| ---- | -------- | ---------------- | --------------- | --------------------------- |
| 1    | put(1,1) | `(空) → 1`       | {1}             | 先頭に追加                  |
| 2    | put(2,2) | `1 → 2,1`        | {1, 2}          | 先頭に追加                  |
| 3    | get(1)   | `2,1 → 1,2`      | {1, 2}          | 1 を先頭へ移動し、1 を返す  |
| 4    | put(3,3) | `1,2 → 3,1`      | {1, 2} → {1, 3} | 最古の 2 を evict、3 を追加 |

返り値: get(1) は `1`。この後 get(2) をすると `-1`

### Complexity

- get, put: O(1)

- Space: O(capacity)

### Edge cases

- 存在しない key の get → -1

- 容量超過で evict

### Interview script

<details><summary>全文(読むだけ。暗記しない)</summary>

This problem gives me the task of building an LRU cache with a fixed size.  
I need to build get and put in O of one, and remove the least recently used key when the cache is full.

Let me confirm a few things first.  
Does a get count as a use?  
Yes.  
What does get return for a missing key?  
Minus one.

The brute force is to use only a map and scan for the oldest key when full.  
That is O of n time.  
That is too slow.

We can do better.  
The key idea is a map plus a doubly linked list: the map finds a node in O of one, and the list moves a
node to the front and removes from the back in O of one.

Here is my plan.  
I keep head and tail references to a doubly linked list; the front is the most recent.  
For each get, I check the map; if the key is there, I move its node to the front and return the value.  
For each put with a known key, I update the value and move the node to the front.  
For each put with a new key, if the cache is full, I remove the tail node and its map entry.  
Then I add the new node at the front and store it in the map.

Let me trace a small example.  
With size two: put one, put two, get one, put three.  
Get one moves key one to the front, so key two is the oldest.  
Put three removes key two.  
That matches.

Time is O of one, because the map and the list steps are all O of one.  
Space is O of the capacity, because I store at most capacity nodes.

For edge cases, get on a missing key returns minus one, and a put over the capacity removes the oldest
key.

</details>

### Follow-up

- Q: How would you build LFU instead?

  → A: I keep a list for each use count and track the minimum count; get and put stay O of one.

- Q: What if each entry belongs to a group, and I must also report the average value of a group in O of
  one?

  → A: I cannot store the average, because an update could not undo the old value.

  I keep a sum and a count per group instead, and every put or evict adds or subtracts one value.

  That is the same trick as the movie rating store in part six.

- Q: Can you use a language built-in?

  → A: In this Haskell version, I combine a mutable hash table with IORef links.

  Data.Map would add a logarithmic lookup cost.

- Q: What if millions of keys must fit?

  → A: The bottleneck is memory, not time.

  I cap the size and let the list evict, or split the cache over many machines by key.

---

## 5. Top K Frequent Elements [(LC347)](https://leetcode.com/problems/top-k-frequent-elements/)

> **Medium** ・ 頻度count+ bucket sort

**言うこと(暗記はこの 2 行)**

> The key idea is: Count, then bucket by count and read from the top.  
> Time is O of n.

### Problem

最頻出 `k` 個の要素を返す。

### Constraints

- `1 <= n <= 10^5`, `-10^4 <= nums[i] <= 10^4`, `1 <= k <= 異なり数`

- 答えは一意に定まることが保証

- O(n log n) ソートでも通るが、bucket sort で O(n) が綺麗

### Brute force

- カウントしてソート: O(n log n)。

### Optimized approach + key idea

- **Key idea (bucket)**: 頻度は `1..n` なのでバケットを n+1 個。

  `bucket[freq]` に要素。

  後ろから k 個。

  O(n)。

- 別解: min-heap of size k → O(n log k)。

### 個別ファイルとサンプル入力

[Solutions/TopKFrequentElements.hs](Solutions/TopKFrequentElements.hs) に解答本体と実行用 `main` がある。

```sh
nix run path:.#p5
```

```text
入力: topKFrequent [1,1,1,2,2,3] 2
期待する出力: [1,2]
```

### Haskell code

```haskell
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
```

### Haskell の読み方

カウントは `HT`、bucket は immutable `Array`。

`HC.toList` は hash table の列挙。

`accumArray (flip (:))` で各 bucket の先頭へ追加する。

同率の順序は不定。

### 解説(なぜ動くか)

- 保つ不変条件: `buckets[f]` には「ちょうど f 回出た値」だけが入る。

  出現回数は必ず 1〜n なので、バケットは n+1 個で足りる。

- 直感: 「回数で並べ替える」のではなく「回数を添字にして置く」。

  counting sort と同じ発想で、比較ソートの O(n log n) を回避する。

- 大きい f から拾うので、k 個集まった時点でそれが最頻の k 個になっている。

- つまずき: `buckets` の長さを n+1 にし忘れる (全要素が同じだと f = n になる)。

- つまずき: 走査は後ろ (大きい count) から。前からだと最少頻度から拾ってしまう。

- 同率の順序は問わなくてよい (問題が答えの一意性を保証している)。

### Trace(値の動き)

入力: `nums = [1, 1, 1, 2, 2, 3]`, `k = 2`

| step | 処理         | 状態                                                 | 動作                   |
| ---- | ------------ | ---------------------------------------------------- | ---------------------- |
| 1    | Counter      | freq = `{1: 3, 2: 2, 3: 1}`                          | 出現回数を数える       |
| 2    | バケット詰め | `buckets[3]=[1]`, `buckets[2]=[2]`, `buckets[1]=[3]` | count を添字に値を置く |
| 3    | f = 6..4     | res = `[]`                                           | 空バケット → skip      |
| 4    | f = 3        | res `[] → [1]`                                       | 1 を追加               |
| 5    | f = 2        | res `[1] → [1, 2]`                                   | len(res) == k → 返す   |

返り値: `[1, 2]`

### Complexity

- Time: O(n)

- Space: O(n)

### Edge cases

- 全要素同じ → 1 個

- k == unique count

### Interview script

<details><summary>全文(読むだけ。暗記しない)</summary>

This problem gives me an array and a number k.  
I need to return the k values with the largest counts.

Let me confirm a few things first.  
Is k at most the number of distinct values?  
Yes.  
Does the order of the answer matter?  
No.

The brute force is to count the values and sort by count.  
That is O of n log n time.  
That is too slow.

We can do better.  
The key idea is bucket sort by count, because every count is between one and n, so bucket f can hold
the values with count f.

Here is my plan.  
I keep a map from value to count.  
I keep a list of buckets, where bucket f holds the values with count f.  
For each distinct value, I add it to its bucket.  
I scan the buckets from the largest count down, and I add values to the result.  
I repeat until I have k values, and at the end, I return the result.

Let me trace a small example.  
With [1, 1, 1, 2, 2, 3] and k two, the count of one is three, of two is two, of three is one.  
Bucket three has one, and bucket two has two.  
I scan down, pick one and two, and stop at k values.  
That matches.

Time is O of n, because the count pass and the bucket pass are both one scan.  
Space is O of n, because the count map and the buckets grow with n.

For edge cases, all equal values give one answer, and k equal to the distinct count returns every value.

</details>

### Follow-up

- Q: Can you guarantee O of n? → A: Yes; the buckets are indexed by count, so there is no sort.

- Q: What if n is huge but k is small?

  → A: The bottleneck is the buckets of size n.

  I can fix it with a min heap of size k, which is O of n log k time and O of k space.

- Q: What if the values come as a stream?

  → A: I keep the count map and a heap of size k, and I update both for each value.

---

## 52. Find Median From Data Stream [(LC295)](https://leetcode.com/problems/find-median-from-data-stream/)

> **Hard** ・ 2ヒープ(max側/min側)で中央値

**言うこと(暗記はこの 2 行)**

> The key idea is: Two heaps: a max heap for the low half, a min heap for the high half.  
> Time is O of log n to add, O of one for the median.

### Problem

ストリームに追加しつつ中央値を返す。

### Constraints

- addNum は最大 5 \* 10^4 回, `-10^5 <= num <= 10^5`

- findMedian は非空時のみ。偶数個は中央2つの平均(float)

- max-heap(下半分)+ min-heap(上半分)を平衡維持 O(log n)/add

### Brute force

- 全保存してソート: O(n log n) per query。

### Optimized approach + key idea

- **Key idea**: Two heaps。`low` (下半分 max heap), `high` (上半分 min heap)。サイズ差 <= 1。

### 個別ファイルとサンプル入力

[Solutions/MedianFinder.hs](Solutions/MedianFinder.hs) に解答本体と実行用 `main` がある。

```sh
nix run path:.#p52
```

```text
入力: addNum 1, addNum 2, addNum 3; findMedian
期待する出力: Just 2.0
```

### Haskell code

```haskell
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
```

### Haskell の読み方

状態を破壊せず、`addNum x oldState` が新しい `MedianFinder` を返す。

`Down Int` により low を max-heap にする。

空ストリームの中央値は `Nothing`、非空なら `Just value`。

### 解説(なぜ動くか)

- 保つ不変条件は 2 つ: (1) low の全要素 ≤ high の全要素、(2) サイズ差は |low| - |high| ∈ {0, 1}。

  この 2 つが守られていれば中央値は常にヒープの頂上にある。

- add の 3 手順が不変条件を守る仕組み: low に push → low の最大を high へ移す(順序の保証)→ high
  が大きくなったら先頭を low へ戻す(バランスの保証)。

- どんな値もまず low → high を必ず経由するので、場合分けなしで順序が保たれるのがこの実装のうまさ。

- 中央値の読み方: 奇数個なら low の頂上、偶数個なら両頂上の平均。

- つまずき 1: `Heap` は min-heap なので low では `Down` で順序を反転する。

  取り出すときは `Down x` をパターンマッチする。

- つまずき 2: 「小さい値なら low、大きい値なら high に直接入れる」と書くと順序が壊れるケースが出る。

### Trace(値の動き)

入力: `add(1)`, `add(2)`, `add(3)`(low は実値で表記)

| step | add | low → high 経由後   | バランス後 low / high | median        |
| ---- | --- | ------------------- | --------------------- | ------------- |
| 1    | 1   | low=[], high=[1]    | low=[1] / high=[]     | 1             |
| 2    | 2   | low=[1], high=[2]   | そのまま(サイズ同じ)  | (1+2)/2 = 1.5 |
| 3    | 3   | low=[1], high=[2,3] | low=[2,1] / high=[3]  | 2             |

返り値: `findMedian() = 2`

### Complexity

- addNum: O(log n)

- findMedian: O(1)

- Space: O(n)

### Edge cases

- 1 要素

- 偶数個 → 2 要素平均

### Interview script

<details><summary>全文(読むだけ。暗記しない)</summary>

This problem gives me a stream of numbers.  
After any add, I need to return the current median.

Let me confirm a few things first.  
Is the median the average of the two middle values when the count is even?  
Are adds and median calls both frequent, so both must be fast?

The brute force is to keep a sorted list and pick the middle value.  
That is O of n time.  
That is too slow.

We can do better.  
The key idea is two heaps: a max heap called low for the smaller half and a min heap called high for
the larger half, with sizes different by at most one.

Here is my plan.  
For each add, I push the number into low.  
I move the top of low into high, so every value in low is at most every value in high.  
If high grows larger than low, I move its top back into low.  
For the median, I check the sizes.  
If low is larger, I return its top. If the sizes are the same, I return the average of the two tops.

Let me trace a small example.  
I add one, then two, then three.  
After one, the median is one.  
After two, low has one and high has two, so the median is one point five.  
After three, low has one and two, and high has three, so the median is two.  
That matches.

Time is O of log n, because each add is a few heap pushes and pops, and the median read is O of one.  
Space is O of n, because the two heaps hold all numbers.

For edge cases, a single number is its own median, and an even count averages the two middle values.

</details>

### Follow-up

- Q: What if the numbers come from a small range?

  → A: Count buckets give O of one for each add and each median read.

- Q: What about the median of a sliding window?

  → A: The bottleneck is removing old values from a heap.

  I can fix it with lazy removal or a sorted structure.

---

## 14. Longest Substring Without Repeating Characters [(LC3)](https://leetcode.com/problems/longest-substring-without-repeating-characters/)

> **Medium** ・ 可変窓 + last-seen map

**言うこと(暗記はこの 2 行)**

> The key idea is: A sliding window with last-seen indexes. Jump the left side past a repeat.  
> Time is O of n.

### Problem

文字が重複しない最長部分文字列の長さ。

### Constraints

- `0 <= len(s) <= 5 * 10^4`, 英数字・記号・空白あり得る

- 部分「文字列」= 連続。空文字なら 0

- 可変窓 + 最終出現 map で O(n)

### Brute force

- 全部分文字列: O(n²) 以上。

### Optimized approach + key idea

- **Key idea**: Sliding window with last-seen map。重複が窓内にあれば左をジャンプ。

### 個別ファイルとサンプル入力

[Solutions/LongestSubstringWithoutRepeatingCharacters.hs](Solutions/LongestSubstringWithoutRepeatingCharacters.hs) に解答本体と実行用 `main` がある。

```sh
nix run path:.#p14
```

```text
入力: lengthOfLongestSubstring "abcabcbb"
期待する出力: 3
```

### Haskell code

```haskell
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
```

### Haskell の読み方

`String` を先頭から一度だけ読む。

last-seen は `hashtables`。

`left' = max left (previous + 1)` とするので、窓の左端は後退しない。

### 解説(なぜ動くか)

- 保つ不変条件: 窓 `[left, right]` の中の文字は常に全て distinct。`last[ch]` は ch を最後に見た index。

- 直感: 重複を見たら左端を「前回のその文字の次」まで一気にジャンプする。1 歩ずつ縮める必要がない。

- つまずき: `last[ch] >= left` の条件が必須。窓の外の古い出現でジャンプすると left
  が後ろに戻ってしまう。

- left は前にしか動かないので、両端合わせて高々 2n 歩 → O(n)。

- つまずき: best の更新は last の更新後に毎ステップ行う (ジャンプした直後の窓も候補)。

### Trace(値の動き)

入力: `s = "abca"`

| step | right, ch | left    | last                   | best    | 動作                               |
| ---- | --------- | ------- | ---------------------- | ------- | ---------------------------------- |
| 1    | 0, a      | 0       | `{}` → `{a: 0}`        | `0 → 1` | 窓 "a"                             |
| 2    | 1, b      | 0       | → `{a: 0, b: 1}`       | `1 → 2` | 窓 "ab"                            |
| 3    | 2, c      | 0       | → `{a: 0, b: 1, c: 2}` | `2 → 3` | 窓 "abc"                           |
| 4    | 3, a      | `0 → 1` | → `{a: 3, b: 1, c: 2}` | 3       | a が窓内 → left ジャンプ、窓 "bca" |

返り値: `3`

### Complexity

- Time: O(n)

- Space: O(min(n, alphabet))

### Edge cases

- 空文字 → 0

- 全部同じ文字 → 1

### Interview script

<details><summary>全文(読むだけ。暗記しない)</summary>

This problem gives me a string.  
I need the length of the longest substring where all characters are distinct.

Let me confirm a few things first.  
Must the characters be next to each other in the string?  
Yes, it is a substring.  
Can the characters be any kind?  
Yes, so I store them in a map.

The brute force is to check every substring for duplicate characters.  
That is O of n squared time.  
That is too slow.

We can do better.  
The key idea is a sliding window with a map from character to its last index, so on a duplicate inside
the window, the left edge jumps past the old position.

Here is my plan.  
I keep a map called last, a left pointer, and a best length.  
I scan the string from left to right with the right pointer.  
For each character, I check if its last index is at or after the left pointer.  
If yes, I move the left pointer to that index plus one. If no, the window grows.  
I update the last index and the best length, and at the end, I return the best.

Let me trace a small example.  
With "abca", the window grows to "abc", so the best is three.  
At the second a, the left pointer jumps to index one.  
The window is "bca", still size three, so the answer is three.  
That matches.

Time is O of n, because each window edge moves at most n steps.  
Space is O of the smaller of n and the alphabet size, because the map stores each distinct character
once.

For edge cases, an empty string gives zero, and all equal characters give one.

</details>

### Follow-up

- Q: What if at most k distinct characters are allowed?

  → A: The same window, but I shrink it when the map size goes over k.

- Q: What if the string is a huge stream?

  → A: The window still works; I keep only the map and the two edges, not the text.

- Q: Why does the left edge never move back?

  → A: The left edge only jumps forward past a duplicate, so each edge moves at most n steps and time
  stays O of n.

---

## 108. Implement Trie [(LC208)](https://leetcode.com/problems/implement-trie-prefix-tree/)

> **Medium** ・ dict の入れ子 + 終端マーク

**言うこと(暗記はこの 2 行)**

> The key idea is: Nested children maps with a separate end-of-word flag.  
> Time is O of word length.

### Problem

`insert(word)` / `search(word)`(完全一致) / `startsWith(prefix)`(前方一致)を持つ木を実装。

### Constraints

- 語は英小文字のみ、語長 `1 <= L <= 2000`、呼び出しは最大 `3 * 10^4` 回

- **`search` と `startsWith` を区別する**のが要件の芯(`app` を入れて `apple` は false)

- 各操作 O(L)。全語の合計長を N として空間 O(N)

### Brute force

- 語の集合を set で持つ: `search` は O(1) だが `startsWith` が全語走査で O(語数 × L)。

### Optimized approach + key idea

- **Key idea**: 1 文字ずつ枝を辿り、共通接頭辞を共有する。

  各ノードの `isWord` が完全な語の終端を表し、子は `Map Char Trie` で別に持つ。

### 個別ファイルとサンプル入力

[Solutions/ImplementTrie.hs](Solutions/ImplementTrie.hs) に解答本体と実行用 `main` がある。

```sh
nix run path:.#p108
```

```text
入力: insert "apple"; insert "app"; search "app"; search "ap"; startsWith "ap"
期待する出力: [True,False,True]
```

### Haskell code

```haskell
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
```

### Haskell の読み方

`Trie Bool (Map Char Trie)` で終端と子を型で分ける。

`trieInsert word oldTrie` は新しい Trie を返すので、その返り値を次の操作へ渡す。

### 解説(なぜ動くか)

- root から辿った文字列が、そのノードの表す接頭辞。途中で子がなければその接頭辞は存在しない。

- `trieSearch` と `trieStartsWith` は `trieWalk` を共有し、終端の `isWord` を確認するかだけが違う。

- `M.alter` で、既存の子を更新するか `emptyTrie` から作るかをまとめる。

  通った枝だけを作り直し、他の枝は共有する。

- `app` と `apple` を同時に保存できるよう、終端と子の有無を分ける。`$` のような特殊キーは不要。

- 英小文字のときは各ノードの子が最大 26 個なので O(L)。一般の文字集合なら O(L log σ)、σ
  は分岐数の最大値。

### Trace(値の動き)

`t = trieInsert "app" emptyTrie` を作る。`isWord` は root,a,ap で False、app で True。

| 操作 | 辿り着くノード | 判定 |
| --- | --- | --- |
| `trieSearch "app" t` | app | isWord=True → True |
| `trieSearch "ap" t` | ap | isWord=False → False |
| `trieStartsWith "ap" t` | ap | ノードがある → True |
| `trieSearch "apple" t` | app の先に l がない | False |

`trieInsert "apple" t` で作った新しい木でも app の終端は True のまま。元の t もそのまま使える。

### Complexity

- insert / search / startsWith: O(L)(L は語長)

- Space: O(N)(N は全語の合計長)

### Edge cases

- 空文字列の `startsWith` は常に True(root に居るだけ)

- 同じ語を 2 回 insert しても `isWord` を True にするだけ

### Interview script

<details><summary>全文(読むだけ。暗記しない)</summary>

This problem gives me the task of building a prefix tree.  
I need insert, search for a full word, and search for a prefix.

Let me confirm a few things first.  
Are the words lowercase letters only?  
Yes.  
Must search tell a full word from a prefix?  
Yes, that is the main point.

The brute force is to keep a set of words.  
Search is O of one, but a prefix check scans every word.  
That is O of the word count times the length.  
That is too slow.

We can do better.  
The key idea is one node per character, so words that share a prefix share the same path.

Here is my plan.  
Each node has a Boolean word flag and a map from characters to child nodes.  
For insert, I walk down the word and create a child when it is missing,
then I set the word flag on the last node and return the updated tree.  
For search, I walk the word; if I fall off I return false, and at the end I check the word flag.  
For startsWith, I do the same walk but I do not check the mark.

Let me trace a small example.  
I insert app, so the tree is a, then p, then p with an end mark.  
Search app walks all three and finds the mark, so it is true.  
Search ap walks two nodes but there is no mark, so it is false.  
StartsWith ap is true.  
That matches.

Time is O of L per call, because I touch one node per character.  
Space is O of N, the total length of all words.

For edge cases, an empty prefix is always true, and inserting the same word twice just sets the mark
again.

</details>

### Follow-up

- Q: Why not a set of words?

  → A: A set cannot answer a prefix query without scanning every word.

  The tree shares prefixes, so a prefix query is O of L.

- Q: How would you use less memory?

  → A: The bottleneck is one map per node.

  I can use a fixed array of twenty-six when the alphabet is small, or compress single-child chains
  into one edge, which is a radix tree.

- Q: How would you support delete?

  → A: I walk down and remove the word flag, then on the way back up I drop each node that has no
  children and no mark.

- Q: How would you build autocomplete on top of this?

  → A: I walk to the prefix node, then I run a small search below it to collect words.

  If I need the top k by score, I store the best score of the subtree in each node and I use a heap
  over the children.

- Q: What if the words come as a stream and never stop?

  → A: Insert is already one word at a time.

  The tree only grows, so I cap it by dropping rare branches, or I shard the tree by first character
  over machines.

---

## 111. Word Search II [(LC212)](https://leetcode.com/problems/word-search-ii/)

> **Hard** ・ Trie を盤面 DFS と同時に降りる

**言うこと(暗記はこの 2 行)**

> The key idea is: Put the words in a trie, then DFS the grid while walking the trie.  
> Time is O of cells times four to the L.

### Problem

文字グリッドと辞書。上下左右に繋げて作れる語を全て返す。1マスは1語の中で再利用不可。

### Constraints

- `1 <= m, n <= 12`、`1 <= 語数 <= 3 * 10^4`、`1 <= 語長 <= 10`

- **語数が多く盤面は小さい** → 「語ごとに盤面を探す」ではなく「盤面を1回歩いて全語を同時に照合」

- 結果は重複なし。1語は1回だけ返す

### Brute force

- 各語で LC79(Word Search)を実行: O(語数 × m × n × 4^L)。語数 3万では通らない。

### Optimized approach + key idea

- **Key idea**: 辞書を Trie にして、**盤面の DFS と Trie の下降を同時に進める**。

  次の文字が Trie に無い枝は即座に切れるので、共通接頭辞を持つ語をまとめて探索できる。

  見つけた語は Trie から外して重複と再訪を防ぐ。

### 個別ファイルとサンプル入力

[Solutions/WordSearchII.hs](Solutions/WordSearchII.hs) に解答本体と実行用 `main` がある。

```sh
nix run path:.#p111
```

```text
入力: findWords ["ab","cd"] ["ab","acd"]
期待する出力: ["acd","ab"]
```

### Haskell code

```haskell
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
```

### Haskell の読み方

盤面は `runST` 内で `'#'` を付けて戻す。

Trie は更新版を再帰の返り値で受け取り、次の隣接マス・開始マスへ渡す。

見つけた語の終端を消し、空になった枝を削除する。

### 解説(なぜ動くか)

- `dfs path node cell` の時点で、今まで踏んだ文字列が `node` の接頭辞に一致する。

  次の文字の子がなければ即終了。

- 終端を見つけたら `reverse path'` を結果へ追加し、その子の終端を False にする。

  同じ語を別経路で見つけても重複しない。

- `'#'` は現在の探索経路だけの訪問済みマーク。戻る時に元の文字へ戻す。入力は英小文字なので衝突しない。

- 探索後の Trie を返して次の探索へ渡すことが大切。古い Trie を使い直すと、消した終端・枝が復活する。

- 終端が False かつ子も空なら枝を `M.delete` で削除する。長い語が残る prefix は削除しない。

- `Map Char Trie` の更新は O(log σ)。英小文字なら σ ≤ 26。盤面の配列コピーも空間に数える。

### Trace(値の動き)

入力: `findWords ["ab","cd"] ["ab","acd"]`。実装の隣接探索順は下・上・右・左。

| step | セル / prefix | 動作 |
| --- | --- | --- |
| 1 | (0,0) / a | a を # にして下へ進む |
| 2 | (1,0) / ac | c を # にして右へ進む |
| 3 | (1,1) / acd | 語を記録、終端を False。戻る時に d,c の枝を刈る |
| 4 | (0,1) / ab | a から右へ。語を記録、終端を False |
| 5 | 復元 | b,a を復元し、使い切った Trie の枝を削除 |

返り値: `["acd","ab"]`。出力順は要件に含まれない。

### Complexity

- Time: O(S + mn × 4 × 3^(L-1) + Z)(英小文字。S は辞書の合計長、L は最大語長、Z は返す語の合計長)

- Space: O(S + mn + L + Z)(Trie、局所的な盤面、探索経路、出力)

- 枝刈りにより、実際の探索はこの上界より小さくなる。

### Edge cases

- 語が盤面に無い文字を含む → root の直下で即 return

- 1マスだけの盤面、1文字の語

### Interview script

<details><summary>全文(読むだけ。暗記しない)</summary>

This problem gives me a character grid and a list of words.  
I need every word I can trace through neighbouring cells without reusing a cell.

Let me confirm a few things first.  
How many words can there be?  
Up to thirty thousand, and the board is at most twelve by twelve.  
Can the same word appear twice in the answer?  
No, once each.

The brute force is to run a single word search for every word.  
That is the word count times the board size times four to the length.  
With thirty thousand words that is far too slow.

We can do better.  
The key idea is to put the dictionary into a prefix tree and walk the board and the tree together, so
words that share a prefix are searched at the same time.

Here is my plan.  
First I build the prefix tree with a Boolean flag at each word end.  
Then I start a search from every cell.  
At each step I look up the current character in the current tree node; if it is missing I stop right
there.
At a word end, I rebuild the current path as a word, add it to the answer, and clear its flag to stop
duplicates.
I mark the cell, I try the four neighbours, and then I restore the cell.  
On the way back, if a tree node has no children left, I remove it from its parent, so the tree shrinks
as I go.

Let me trace a small example.  
The board is a and b on the top row, c and d on the bottom, and the words are ab and acd.  
From a I go down to c, then right to d, and find acd.  
I restore those cells and explore right from a to b to find ab.  
The answer contains acd and ab.  
That matches.

Time is the board size times four times three to the length minus one in the worst case, but the
pruning makes it much faster in practice.
Space includes the trie, a local board array, the search path, and the returned words.

For edge cases, a word with a character that is not on the board stops at the root.

</details>

### Follow-up

- Q: Why is this faster than one search per word?

  → A: The bottleneck in the brute force is repeating the same prefix walk for every word.

  The tree walks each shared prefix once.

- Q: Why clear the word flag in the tree instead of using a set?

  → A: A set also works for duplicates, but removing the node lets me prune the branch, which cuts the
  search itself.

- Q: What if the board is huge instead of the dictionary?

  → A: Then the tree helps less.

  I would search from each cell only when its character starts some word, and I would cut by the
  longest word length.

- Q: What if a cell could be reused?

  → A: The search would not terminate on its own, so I would bound it by the longest word length
  instead of by the visited mark.

- Q: Why do you use the board itself as the visited mark?

  → A: It avoids a set lookup per step.

  If I could not modify the input, I would keep a visited set of cells.

---

## 101. Sliding Window Maximum [(LC239)](https://leetcode.com/problems/sliding-window-maximum/)

> **Hard** ・ 単調減少 deque に index

**言うこと(暗記はこの 2 行)**

> The key idea is: A deque of indexes with decreasing values. The front is the max.  
> Time is O of n.

### Problem

配列 `nums` と窓幅 `k`。窓を1つずつ滑らせたときの各位置の最大値を配列で返す。

### Constraints

- `1 <= len(nums) <= 10^5`, `1 <= k <= len(nums)`

- **O(n) が要求**(窓ごとに max を取ると O(n·k) で通らない)

- 値は負もあり。deque に `(index, value)` を入れる(index で窓外かどうかを判定するため)

### Brute force

- 各窓で max: O(n·k)。n=10^5, k=10^4 で 10^9 回。

- max-heap に (値, index) を積み、頭が窓外なら捨てる: O(n log n)。これでも通るが O(n) には届かない。

### Optimized approach + key idea

- **Key idea**: 単調減少 deque。

  **新しい値より小さい末尾は、もう二度と最大になれない**ので捨ててよい(新しい方が大きく、
  かつ長く窓に残る)。

  頭が常に窓内の最大。

### 個別ファイルとサンプル入力

[Solutions/SlidingWindowMaximum.hs](Solutions/SlidingWindowMaximum.hs) に解答本体と実行用 `main` がある。

```sh
nix run path:.#p101
```

```text
入力: maxSlidingWindow [1,3,-1,-3,5,3,6,7] 3
期待する出力: [3,3,5,5,6,7]
```

### Haskell code

```haskell
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
```

### Haskell の読み方

deque は `Data.Sequence (Int, Int)` で `(index, value)` を保持する。

リストの `!!` で値を引き直さない。

答えは逆順で蓄積して最後に戻す。

### 解説(なぜ動くか)

- 保つ不変条件: deque の index は**昇順**、対応する値は**降順**。

  この2つが同時に成り立つので、頭が「窓内で最大かつ最も古い」候補になる。

- 捨てて良い理由が本質。

  `nums[j] < nums[i]` で `j < i` なら、j が窓に居る間は必ず i も窓に居る。

  **より大きくてより長く残る値**があるので、j は二度と答えにならない。

- 各 index は「1回 append、高々1回 pop」しかされないので、内側の while があっても全体で O(n)。

  償却計算量の典型例。

- `(index, value)` を入れるのは、index で窓から出たかを判定し、value で大小を比較するため。

  値だけだと同値の区別がつかない。

- つまずき: `value < x` を `value <= x` にしても答えは合う(同値も捨てる)。

  ただし「同じ値をどう扱うか」を聞かれたら説明できるようにしておく。

- つまずき: `expire` は `j <= i - k` の先頭要素を除く。今の窓は `[i-k+1, i]`。

- つまずき: 結果を積み始めるのは `i >= k - 1` から。最初の窓が埋まる前に積むと長さが合わない。

### Trace(値の動き)

入力: `nums = [1, 3, -1, -3, 5]`, `k = 3`

| step | i   | n   | pop する末尾        | q (index) | q の値      | popleft | res       |
| ---- | --- | --- | ------------------- | --------- | ----------- | ------- | --------- |
| 1    | 0   | 1   | なし                | `[0]`     | `[1]`       | —       | `[]`      |
| 2    | 1   | 3   | index0(1<3)         | `[1]`     | `[3]`       | —       | `[]`      |
| 3    | 2   | -1  | なし(3>-1)          | `[1,2]`   | `[3,-1]`    | —       | `[3]`     |
| 4    | 3   | -3  | なし(-1>-3)         | `[1,2,3]` | `[3,-1,-3]` | —       | `[3,3]`   |
| 5    | 4   | 5   | index3,2,1 を全 pop | `[4]`     | `[5]`       | —       | `[3,3,5]` |

返り値: `[3, 3, 5]`

### Complexity

- Time: O(n)(各 index は高々1回 push、1回 pop)

- Space: O(k) 補助空間。返す結果を含めると O(n)。

### Edge cases

- `k == 1` → 入力そのものが答え

- `k == len(nums)` → 全体の最大が1個

- 全て同じ値、単調増加(deque は常に長さ1)、単調減少(deque が k まで伸びる)

### Interview script

<details><summary>全文(読むだけ。暗記しない)</summary>

This problem gives me an array and a window width.  
I need the maximum of the window at every position.

Let me confirm a few things first.  
How large can the array be?  
Up to one hundred thousand, so I am aiming for O of n.  
Can the values be negative?  
Yes.\

The brute force takes the maximum of every window.  
That is O of n times k.  
With those sizes that is far too slow.  
A max heap with the index brings it to O of n log n, which is closer but still not linear.

We can do better.  
The key idea is that when a new value arrives, every smaller value already in the window is useless,
because the new one is both larger and stays longer.

Here is my plan.  
I keep a deque of indexes whose values are decreasing.  
For each index, I pop from the back while the back value is smaller than the current value, then I push
the current index.
If the front index has just left the window, I pop it from the front.  
Once the first window is full, the front index always holds the maximum, so I append its value to the
answer.

Let me trace a small example.  
The array is one, three, minus one, minus three, five, and k is three.  
Three pushes one out, so the first window reports three.  
The second window still reports three.  
Then five clears the whole deque and the last window reports five.  
The answer is three, three, five.  
That matches.

Time is O of n, because each index enters and leaves the deque at most once.  
Space is O of k, because the deque only holds candidates inside the window.

For edge cases, a window of one returns the input, and a window as wide as the array returns a single
maximum.

</details>

### Follow-up

- Q: Why is it O of n when there is a loop inside a loop?

  → A: Each index is pushed once and popped once over the whole run, so the total work is linear.

  That is an amortised argument, not a per step one.

- Q: Why store indexes and not values?

  → A: I need to know when the front has left the window, and that is a position, not a value.

- Q: How would you also return the minimum of each window?

  → A: I keep a second deque with the opposite order, increasing instead of decreasing.

- Q: What if the values arrive as a stream?

  → A: The algorithm is already online.

  It only looks at the current value and the deque, so it emits an answer as soon as each window fills.

- Q: What if the window width changes over time?

  → A: The deque still works when the window only grows or only shrinks from the front.

  For arbitrary changes I would use a sorted structure or a heap keyed by value with lazy deletion.

- Q: What if you also had to remove elements from the middle?

  → A: A deque cannot do that.

  I would move to a heap with lazy deletion or a balanced tree, which costs O of log n per step.

---

## 93. Word Break [(LC139)](https://leetcode.com/problems/word-break/)

> **Medium** ・ dp[i]=どれかの語で i まで作れる

**言うこと(暗記はこの 2 行)**

> The key idea is: From each reachable prefix, walk the trie and mark every word end as reachable.  
> Time is O of n times the longest word.

### Problem

`s` を辞書の単語で分割できるか。

### Constraints

- `1 <= len(s) <= 300`, 辞書 `<= 1000` 語, 各語 `<= 20`

- **辞書の語は何回でも再利用可**

- dp[i] = ある語 w で dp[i-len(w)] が true かつ末尾一致 O(n^2)

### Brute force

- 全分割 DFS: 指数的。

### Optimized approach + key idea

- `dp[i]` = s の先頭 i 文字を辞書語に分割できるか。`dp[0] = True`。

- 到達済みの start から Trie を歩き、終端まで来たらその位置の dp を True にする。

- 辞書に合う prefix が途切れたら止める。辞書を hash set にして部分文字列を何度も作る処理は不要。

### 個別ファイルとサンプル入力

[Solutions/WordBreak.hs](Solutions/WordBreak.hs) に解答本体と実行用 `main` がある。

```sh
nix run path:.#p93
```

```text
入力: wordBreak "leetcode" ["leet","code"]
期待する出力: True
```

### Haskell code

```haskell
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
```

### Haskell の読み方

辞書を Trie にして、到達できる prefix の後ろだけを文字ごとに照合する。

`tails s` は suffix を共有するため、毎回先頭から `drop` しない。

### 解説(なぜ動くか)

- start より前を分割でき、start から end が辞書語なら、end まで全体を分割できる。

- start を昇順に見るので、到達可能な位置を新たに見つけた後、その後ろも探索できる。

- 単語の再利用は制限しない。辞書の Trie は更新しない。

- `tails s` の各 suffix は元のリストの末尾を共有する。

  `String` の `!!` や毎回の `drop start` による余分な走査を避ける。

- 空文字列は True、空の辞書で非空の文字列は False。空の辞書語は遷移として使わない。

### Trace(値の動き)

入力: `s = "leetcode"`, `wordDict = ["leet", "code"]`

| i    | 現在の値   | dp/変数の変化(前 → 後) | 動作(短い日本語)                    |
| ---- | ---------- | ---------------------- | ----------------------------------- |
| 1〜3 | "l"〜"lee" | dp[1..3]: False のまま | どの切れ目でも語にならない          |
| 4    | "leet"     | dp[4]: False → True    | j=0: dp[0] かつ "leet" が辞書にある |
| 5    | "leetc"    | dp[5]: False のまま    | dp[4] は True だが "c" は語でない   |
| 6    | "leetco"   | dp[6]: False のまま    | 語なし                              |
| 7    | "leetcod"  | dp[7]: False のまま    | 語なし                              |
| 8    | "leetcode" | dp[8]: False → True    | j=4: dp[4] かつ "code" が辞書にある |

返り値: `True`

### Complexity

- Time: O(S + nL)(英小文字。S は辞書の合計長、L は最大語長)

- Space: O(S + n)(Trie と dp)

- 一般の文字集合なら Trie 操作に log σ が掛かる。

  `wordBreakCount` の巨大な `Integer` の演算コストは別途加わる。

### Edge cases

- 完全にカバーできない → False

- 単語の重複使用可

### Interview script

<details><summary>全文(読むだけ。暗記しない)</summary>

This problem gives me a string and a word list.  
I need to check if the whole string can be split into dictionary words, with reuse allowed.

The brute force tries every split, which takes exponential time.

The key idea is dynamic programming with a trie.  
DP at i means the first i characters can be split.  
I start with DP at zero true.  
For each reachable start, I walk the trie along the remaining characters.  
Whenever I reach a word end, I mark that position reachable.  
When there is no matching child, I stop that walk.  
At the end, I return DP at n.

For leetcode with leet and code, DP at four becomes true, then DP at eight becomes true.

Time is O of S plus n times L for a fixed alphabet.  
S is the total dictionary length and L is the longest word length.  
Space is O of S plus n for the trie and the DP table.

</details>

### Follow-up

- Q: Return one actual split.

  → A: `wordBreakOne` keeps a suffix DP of `Maybe [String]`.

  A matching word is prepended to a successful split of the remainder. `Nothing` means impossible; an
  empty suffix is `Just []`.

- Q: Make the cut scan faster.

  → A: The basic Haskell implementation already uses a Trie.

  It stops at the first missing character and never walks past the longest dictionary word.

- Q: Count all splits.

  → A: `wordBreakCount` changes the suffix DP value to `Integer`; sum the counts at every matching word
  end.

  The empty suffix has one split.

  Duplicate dictionary entries do not add duplicate splits.

- The lazy `Array` in the two follow-ups is safe because every edge goes from i to a strictly larger
  index j.

  Each table entry is shared and computed at most once.

---

## 86. House Robber [(LC198)](https://leetcode.com/problems/house-robber/)

> **Medium** ・ dp[i]=max

**言うこと(暗記はこの 2 行)**

> The key idea is: The best up to a house is the max of skipping it, or robbing it plus the best from two houses back.  
> Time is O of n time, O of one space.

### Problem

隣接する家を同時に盗めない。最大金額。

### Constraints

- `1 <= n <= 100`, `0 <= nums[i] <= 400`

- 隣どうしは盗めない

- dp[i] = max(dp[i-1], dp[i-2] + a[i]) → 変数2個 O(n)/O(1)

### Brute force

- 全 rob/skip: 指数的。

### Optimized approach + key idea

- **Key idea**: `dp[i] = max(dp[i-1], dp[i-2] + nums[i])`。2 変数。

### 個別ファイルとサンプル入力

[Solutions/HouseRobber.hs](Solutions/HouseRobber.hs) に解答本体と実行用 `main` がある。

```sh
nix run path:.#p86
```

```text
入力: rob [2,7,9,3,1]
期待する出力: 12
```

### Haskell code

```haskell
rob :: [Int] -> Int
rob = snd . foldl' step (0, 0)
  where
    step (!prev2, !prev1) x =
      let !best = max prev1 (prev2 + x)
      in (prev1, best)
```

### Haskell の読み方

`foldl'` で `(prev2, prev1)` を更新する。

`BangPatterns` の `!` で足し算の遅延を溜めず、補助空間 O(1) を保つ。

### 解説(なぜ動くか)

- dp at i(コードでは prev1)= 「家 i までで盗める最大額」。prev2 はその 1 つ前。

- 遷移: dp[i] = max(dp[i-1], dp[i-2] + nums[i])。

  「盗まない(前の最大を保つ)」か「盗む(2 つ前の最大 + 今の額)」の 2 択。

- 組 `(prev2, prev1)` を新しい組へ更新し、dp 配列を置き換える。新しい値は古い組から計算する。

- つまずき: dp[i-1] を選ぶ = 「家 i-1 を必ず盗む」ではない。

  dp は「i までの最大」であって、どの家を盗んだかは持っていない。

### Trace(値の動き)

入力: `nums = [2, 7, 9, 3, 1]`

| i | 現在の値 | dp/変数の変化(前 → 後) | 動作(短い日本語) |
|---|---|---|---|
| 0 | 2 | prev2=0, prev1=0 → 0, 2 | max(0, 0+2)=2 |
| 1 | 7 | 0, 2 → 2, 7 | max(2, 0+7)=7 |
| 2 | 9 | 2, 7 → 7, 11 | max(7, 2+9)=11 |
| 3 | 3 | 7, 11 → 11, 11 | max(11, 7+3)=11 |
| 4 | 1 | 11, 11 → 11, 12 | max(11, 11+1)=12 |

返り値: `12`

### Complexity

- Time: O(n)

- Space: O(1)

### Edge cases

- n = 1 → nums[0]

- 全部同じ値

### Interview script

<details><summary>全文(読むだけ。暗記しない)</summary>

This problem gives me a row of houses with money values.  
I need to find the maximum money I can rob without robbing two houses next to each other.

Let me confirm a few things first.  
Can I rob two houses that sit next to each other?  
Are all values zero or positive?

The brute force is to check every rob or skip choice.  
That is O of two to the n time.  
That is too slow.

We can do better.  
The key idea is that the best up to a house is the max of skipping it, or robbing it and adding the
best from two houses back.

Here is my plan.  
I keep two values called prev1 and prev2 for the best up to the previous two houses.  
I scan the array from left to right.  
For each house, I update the best to the max of prev1 and the value plus prev2.  
At the end, I return prev1.

Let me trace a small example.  
For [2, 7, 9, 3, 1], I rob two, nine, and one.  
That sum is twelve.  
That matches.

Time is O of n, because I scan the array once.  
Space is O of one, because I keep only two values.

For edge cases, a single house returns its value.

</details>

### Follow-up

- Q: What if the houses form a circle?

  → A: That is House Robber II.

  I run the same scan twice, once without the first house and once without the last, and return the max.

- Q: How would you return which houses I rob?

  → A: I store the rob or skip choice at each house, then walk back from the end.

- Q: What if values can be negative?

  → A: I can skip every house, so the answer is at least zero.

  The same transition works, because skip keeps the previous best.

---

## 95. Partition Equal Subset Sum [(LC416)](https://leetcode.com/problems/partition-equal-subset-sum/)

> **Medium** ・ 部分和=total/2 の 0/1 ナップサック

**言うこと(暗記はこの 2 行)**

> The key idea is: 0/1 knapsack: can a subset make half the total? Go through the sums from high to low so each number is used once.  
> Time is O of n times the target.

### Problem

配列を合計の等しい 2 つに分けられるか。

### Constraints

- `1 <= n <= 200`, `1 <= nums[i] <= 100` → 総和 <= 2*10^4

- **総和が奇数なら即 false**

- 目標 = 総和/2 の 0/1 部分和 DP。O(n × sum/2)

### Brute force

- 全部分集合: 指数的。

### Optimized approach + key idea

- **Key idea**: Subset-sum DP。`total` 奇数なら False。`target = total/2` を作れるか。

### 個別ファイルとサンプル入力

[Solutions/PartitionEqualSubsetSum.hs](Solutions/PartitionEqualSubsetSum.hs) に解答本体と実行用 `main` がある。

```sh
nix run path:.#p95
```

```text
入力: canPartition [1,5,11,5]
期待する出力: True
```

### Haskell code

```haskell
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
```

### Haskell の読み方

`STUArray Int Bool` を `target` から `x` へ降順に更新する。

外側の `forM_` が「各数を一度」、内側の降順が「同じ数を使い回さない」を表す。

### 解説(なぜ動くか)

- 「2 等分できる」⇔「合計 / 2 の部分集合が作れる」。合計が奇数なら即 False。

- dp at s = 「今までの数の部分集合で和 s が作れるか」。dp[0] = True から出発する。

- 遷移: 数 n を見たら dp[s] |= dp[s - n]。True の集合が「+n だけずらしたコピー」の分だけ増えていく。

- 和を target から n へ降順に回すのが 0/1 ナップサックの肝: 昇順だと同じ数を同じループ内で 2
  回使ってしまう。

- つまずき: 逆順ループを昇順に書いてしまうミスが最頻。

  ビット集合(bits |= bits << n)にすると大幅に速くできる。

### Trace(値の動き)

入力: `nums = [1, 5, 11, 5]`(target = 11)

| i | 現在の値 | dp/変数の変化(前 → 後) | 動作(短い日本語) |
|---|---|---|---|
| 0 | 1 | True の和 {0} → {0, 1} | dp[1] = dp[0] |
| 1 | 5 | {0, 1} → {0, 1, 5, 6} | dp[5], dp[6] が True に |
| 2 | 11 | {0, 1, 5, 6} → {0, 1, 5, 6, 11} | dp[11] = dp[0] で目標到達 |
| 3 | 5 | {0, 1, 5, 6, 11} → {0, 1, 5, 6, 10, 11} | dp[10] = dp[5] など |

返り値: `True`(dp[11])

### Complexity

- Time: O(n * target)

- Space: O(target)

### Edge cases

- 合計が奇数 → False

- 単一要素 → False

### Interview script

<details><summary>全文(読むだけ。暗記しない)</summary>

This problem gives me an array of positive numbers.  
I need to check if it can be split into two subsets with the same sum.

Let me confirm a few things first.  
If the total sum is odd, is it always false?  
Otherwise, do I look for a subset whose sum is half the total?

The brute force is to check every subset.  
That is O of two to the n time.  
That is too slow.

We can do better.  
The key idea is a subset-sum dp, where dp at s is true when some subset has sum s, and I only need dp
at half the total.

Here is my plan.  
First, I check the total; if it is odd, I return false.  
I keep a dp table of size target plus one, with dp at zero true.  
I scan the numbers one by one.  
For each number, I scan the sums from the target down to the number, so each number is used at most
once.
If dp at the sum minus the number is true, I mark dp at the sum true.  
At the end, I return dp at the target.

Let me trace a small example.  
For [1, 5, 11, 5], the total is twenty-two, so the target is eleven.  
The number eleven alone marks dp at eleven true.  
That matches.

Time is O of n times the target, because each number updates the dp table once.  
Space is O of the target, because I keep one dp table.

For edge cases, an odd total returns false, and a single number cannot be split.

</details>

### Follow-up

- Q: What about splitting into k equal subsets?

  → A: That is a harder search problem.

  I fill k buckets with backtracking and drop repeated states early.

- Q: Can you make the dp faster?

  → A: The bottleneck is the scan over all sums for each number.

  I can fix it with a bit set, where one shift and one or update all sums at once.

- Q: How would you return the two subsets?

  → A: I store, for each sum, the number that marked it, then walk back from the target.

---

## 94. Longest Increasing Subsequence [(LC300)](https://leetcode.com/problems/longest-increasing-subsequence/)

> **Medium** ・ patience/二分探索で O

**言うこと(暗記はこの 2 行)**

> The key idea is: Keep the smallest tail for each length. Binary search where each number goes.  
> Time is O of n log n.

### Problem

最長厳密増加部分列の長さ。

### Constraints

- `1 <= n <= 2500`, `-10^4 <= nums[i] <= 10^4`

- **strictly increasing**(等しいのは不可)。連続でなくてよい

- n^2 DP でも通るが、follow-up の patience + 二分探索 O(n log n) まで言う

### Brute force

- 全部分列: 指数的。

### Optimized approach + key idea

- **Key idea (O(n log n))**: patience sorting。`tails` に bisect で置き換え。

### 個別ファイルとサンプル入力

[Solutions/LongestIncreasingSubsequence.hs](Solutions/LongestIncreasingSubsequence.hs) に解答本体と実行用 `main` がある。

```sh
nix run path:.#p94
```

```text
入力: lengthOfLIS [10,9,2,5,3,7,101,18]
期待する出力: 4
```

### Haskell code

```haskell
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
```

### Haskell の読み方

tails は二分探索するので `STUArray` を使う。

`lowerBound x 0 size` は半開区間 `[0, size)` で x 以上の最初の位置を返す。

リストの `!!` は使わない。

### 解説(なぜ動くか)

- tails at k = 「長さ k+1 の増加部分列の、いちばん小さい末尾値」。tails は常に昇順に並ぶ。

- 新しい数 n が全末尾より大きければ append(最長を 1 伸ばせる)。

  そうでなければ lowerBound で「n 以上の最初の末尾」を n に置き換える(同じ長さを、
  より低い末尾で持ち直す = 後で伸ばしやすくする)。

- 置き換えでは長さは変わらないが、状態は必ず「同じか有利」になる。だから len(tails) が常に LIS 長。

- lowerBound なので等しい値は置き換えになり、strictly increasing が守られる。

- つまずき: tails の中身は LIS そのものではない(長さだけが正しい)。

  列の復元には親リンク付きの O(n²) DP などが要る。

### Trace(値の動き)

入力: `nums = [10, 9, 2, 5, 3, 7, 101, 18]`

| i | 現在の値 | dp/変数の変化(前 → 後) | 動作(短い日本語) |
|---|---|---|---|
| 0 | 10 | [] → [10] | 末尾に追加 |
| 1 | 9 | [10] → [9] | 10 を置き換え |
| 2 | 2 | [9] → [2] | 9 を置き換え |
| 3 | 5 | [2] → [2, 5] | 末尾に追加 |
| 4 | 3 | [2, 5] → [2, 3] | 5 を置き換え |
| 5 | 7 | [2, 3] → [2, 3, 7] | 末尾に追加 |
| 6 | 101 | [2, 3, 7] → [2, 3, 7, 101] | 末尾に追加 |
| 7 | 18 | [2, 3, 7, 101] → [2, 3, 7, 18] | 101 を置き換え |

返り値: `4`(len(tails))

### Complexity

- Time: O(n log n)

- Space: O(n)

### Edge cases

- 単調減少 → 1

- 単調増加 → n

### Interview script

<details><summary>全文(読むだけ。暗記しない)</summary>

This problem gives me an array.  
I need to find the length of the longest strictly increasing subsequence, where the values need not sit
next to each other.

Let me confirm a few things first.  
Must it be strictly increasing, with no equal pair?  
Do I return only the length?

The brute force is to check every subsequence.  
That is O of two to the n time.  
Even the simple dp with the best length ending at each index is O of n squared.  
That is too slow.

We can do better.  
The key idea is patience sorting: I keep a tails list, where tails at k is the smallest tail of an
increasing subsequence of length k plus one.

Here is my plan.  
I keep a list called tails, starting empty.  
I scan the array from left to right.  
For each number, I check its position in tails with binary search.  
If it is larger than every tail, I add it at the end; otherwise, I update the first tail that is not
smaller.
At the end, I return the length of tails.

Let me trace a small example.  
For [10, 9, 2, 5, 3, 7, 101, 18], tails goes [10], [9], [2], [2, 5], [2, 3], [2, 3, 7], [2, 3, 7, 101],
[2, 3, 7, 18].
Its length is four.  
That matches.

Time is O of n log n, because each number needs one binary search.  
Space is O of n, because I keep the tails list.

For edge cases, a decreasing array gives one, and an increasing array gives n.

</details>

### Follow-up

- Q: Can you return the subsequence too?

  → A: With the n squared dp, I store a parent link at each index and walk back from the best end.

- Q: Why is replacing a tail safe?

  → A: The length does not change, and a smaller tail is never worse, because more numbers can grow the
  run later.

- Q: What about the longest non-decreasing subsequence?

  → A: I use bisect right instead of bisect left, so equal values can grow the run.

---

## 45. Binary Tree Maximum Path Sum [(LC124)](https://leetcode.com/problems/binary-tree-maximum-path-sum/)

> **Hard** ・ DFS: 各節点で「上に返す値」と「答え」を分離

**言うこと(暗記はこの 2 行)**

> The key idea is: DFS returns the best one-sided path. At each node, update the answer with left plus node plus right.  
> Time is O of n time, O of height space.

### Problem

任意 2 ノード間のパスでノード値合計の最大。

### Constraints

- `1 <= n <= 3 * 10^4`, `-1000 <= val <= 1000`(**負あり**)

- path は1ノード以上・枝分かれ不可。全部負なら最大の1ノード

- 各ノードで「上に返す片側値(負なら0)」と「答え=左+自+右」を分離

### Brute force

- 各ノードで両側 path: O(n²)。

### Optimized approach + key idea

- **Key idea**: 各節点の DFS は `(片側 gain, 部分木内の best)` を返す。

  best は左右の best と `value + max 0 leftGain + max 0 rightGain` の最大。

  親への gain では片側だけを選ぶ。

### 個別ファイルとサンプル入力

[Solutions/BinaryTreeMaximumPathSum.hs](Solutions/BinaryTreeMaximumPathSum.hs) に解答本体と実行用 `main` がある。

```sh
nix run path:.#p45
```

```text
入力: Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)
期待する出力: Just 6
```

### Haskell code

```haskell
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
```

### Haskell の読み方

`gain` は `(親へ返す片側の和, 部分木内の最大値)` の組を返す。

共有変数は不要。

空の木の最大値は `Nothing`、非空なら `Just sum`。

### 解説(なぜ動くか)

- 核心は二役の分離: 「親に返す値(片側だけの最良の鎖)」と「答えの候補(左 + 自分 + 右
  の山型パス)」は別物。

- 保つ不変条件: gain(node) = そのノードから下へ一直線に伸びる鎖の最大和。

  負なら 0 に丸めて「伸ばさない」を表す。

- 値の流れ: 葉から上へ gain が積み上がり、各ノードで「ここを頂点とする山」を best と比較する。

  親へ山を返してはいけない(パスが枝分かれしてしまうため)。

- つまずき 1: 負の gain を 0 に丸め忘れ、損な枝を含めてしまう。

- つまずき 2: 部分木の best を 0 にすると全部負の木で壊れる。

  空だけを Nothing にし、実在する節点の値を候補にする。

- つまずき 3: 返り値に left + right を入れてしまう(返すのは片側 + 自分だけ)。

### Trace(値の動き)

入力: `Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)`。

| 節点 | 左右の gain | この節点を通る候補 | 親へ返す `(gain,best)` |
| --- | --- | --- | --- |
| Empty | — | なし | `(0,Nothing)` |
| 1 | 0 / 0 | 1 | `(1,Just 1)` |
| 3 | 0 / 0 | 3 | `(3,Just 3)` |
| 2 | 1 / 3 | 1+2+3=6 | `(5,Just 6)` |

返り値: `Just 6`。親に返す gain=5 と木の中の最大=6 を分ける。

### Complexity

- Time: O(n)

- Space: O(h)

### Edge cases

- 1 ノード → その値

- 全部負

### Interview script

<details><summary>全文(読むだけ。暗記しない)</summary>

This problem gives me a binary tree, and the values can be negative.  
I need the maximum path sum, where a path is any chain of connected nodes, and each node is used at
most once.

Let me confirm a few things first.  
Can values be negative?  
Must the path have at least one node?

The brute force is to check every path through every node.  
That is O of n squared time.  
That is too slow.

We can do better.  
The key idea is a DFS that returns the best one-side gain a parent can use, while it also updates the
best answer with the full path through each node.

Here is my plan.  
Each DFS call returns the one-sided gain and the best path in that subtree.  
I call the same function on the left child and the right child.  
For each side, I check the gain; if it is negative, I use zero.  
I take the largest of both subtree best values and the node value plus both gains.  
I return that best value together with the node value plus the larger gain, because a parent can use
only one side.

Let me trace a small example.  
The root is two, with left child one and right child three.  
At the root, the left gain is one and the right gain is three.  
Best becomes one plus two plus three, which is six.  
That matches.

Time is O of n, because I visit each node once.  
Space is O of h, because the recursion goes as deep as the tree height.

For edge cases, a single node returns its own value, and an all-negative tree returns the largest
single value.

</details>

### Follow-up

- Q: Can you return the path itself, not just the sum?

  → A: Yes; I store the best top node, and I keep the best child chain at each node to build the path
  back.

- Q: Can you do it without recursion?

  → A: Yes, with a stack in post order, because each node needs its children first.

---

## 21. Daily Temperatures [(LC739)](https://leetcode.com/problems/daily-temperatures/)

> **Medium** ・ 単調減少スタックに index

**言うこと(暗記はこの 2 行)**

> The key idea is: A stack of indexes with decreasing temperatures. A warmer day pops them and fills in their answers.  
> Time is O of n.

### Problem

各日について「次に暖かくなる日まで」の日数。なければ 0。

### Constraints

- `1 <= n <= 10^5`, `30 <= temp <= 100`

- より暖かい日が無ければ 0

- 単調減少スタックに index → 各 index は push/pop 1回ずつ O(n)

### Brute force

- 各 i から後ろを線形探索: O(n²)。

### Optimized approach + key idea

- **Key idea**: Monotonic decreasing stack of index。暖かい日が来たら pop して差を記録。

### 個別ファイルとサンプル入力

[Solutions/DailyTemperatures.hs](Solutions/DailyTemperatures.hs) に解答本体と実行用 `main` がある。

```sh
nix run path:.#p21
```

```text
入力: dailyTemperatures [73,74,75,71,69,72,76,73]
期待する出力: [1,1,4,2,1,1,0,0]
```

### Haskell code

```haskell
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
```

### Haskell の読み方

stack の先頭が top。

`(index, temperature)` を一緒に持ち、答えは `STUArray` に書く。

温度の参照にリストの `!!` を使わない。

### 解説(なぜ動くか)

- 保つ不変条件: スタックの中は「まだ答えが出ていない日」の index だけで、温度は底から top へ単調減少。

- 新しい日 i が来たとき、top より暖かい間 pop する。

  pop された日 j にとって i が「最初の暖かい日」(間の日はすべて j 以下の温度だから)。

- 各 index は push 1回・pop 最大1回 → while が入れ子でも合計 O(n)(償却)。

- つまずき: 温度そのものを積むと日数差が出せない。index を積み、`i - j` で日数を得る。

- つまずき: 等しい温度は pop しない(strictly warmer)。比較は `<` の向きに注意。

### Trace(値の動き)

入力: `temperatures = [73, 74, 75, 71, 76]`

| step | i (温度) | stack (index) | ans | 動作 |
|---|---|---|---|---|
| 1 | 0 (73) | `[] → [0]` | `[0,0,0,0,0]` | push |
| 2 | 1 (74) | `[0] → [1]` | `[1,0,0,0,0]` | 73<74 → pop 0, ans[0]=1-0 |
| 3 | 2 (75) | `[1] → [2]` | `[1,1,0,0,0]` | 74<75 → pop 1, ans[1]=1 |
| 4 | 3 (71) | `[2] → [2, 3]` | 変化なし | 75>71 → push のみ |
| 5 | 4 (76) | `[2, 3] → [4]` | `[1,1,2,1,0]` | 71, 75 を pop, ans[3]=1, ans[2]=2 |

返り値: `[1, 1, 2, 1, 0]`

### Complexity

- Time: O(n)

- Space: O(n)

### Edge cases

- 単調減少 → 全部 0

- 全部同じ → 全部 0

### Interview script

<details><summary>全文(読むだけ。暗記しない)</summary>

This problem gives me an array of daily temperatures.  
I need to count, for each day, the days until a warmer day, with zero if there is none.

Let me confirm a few things first.  
Is the answer a count of days, not a temperature?  
Yes.  
Must the later day be strictly warmer?  
Yes.\

The brute force is to scan forward from each day until a warmer day.  
That is O of n squared time.  
That is too slow.

We can do better.  
The key idea is a decreasing stack of indexes: when a warmer day comes, it answers all cooler days
waiting on the stack.

Here is my plan.  
I keep a stack of indexes and an answer array of zeros.  
I scan the days from left to right.  
While the day at the top of the stack is cooler than day i, I pop it and set its answer to i minus that
index.
Then I push i.  
At the end, I return the answer array.

Let me trace a small example.  
For [73, 74, 75, 71, 76], day one answers day zero with one.  
Day two answers day one with one.  
Day four answers day three with one, and it pops day two with two.  
That matches.

Time is O of n, because each index is pushed and popped at most once.  
Space is O of n, because the stack can hold all indexes.

For edge cases, temperatures that only go down, or all the same, give all zeros.

</details>

### Follow-up

- Q: What if the array is circular?

  → A: I scan it two times with index modulo n, and I push only on the first pass.

- Q: Can you use less memory?

  → A: The bottleneck is the stack.

  I can scan from right to left and jump with the answer array itself, so extra space is O of one.

- Q: What if the temperatures come as a stream?

  → A: The same stack works, because I only keep days that still wait for an answer.

---

## 54. Combination Sum [(LC39)](https://leetcode.com/problems/combination-sum/)

> **Medium** ・ 同じ要素再利用、index で枝刈り

**言うこと(暗記はこの 2 行)**

> The key idea is: Backtrack over the remaining candidates so each combination is built once. Keep the same candidate list to reuse a number.  
> Time is Exponential, cut by the target.

### Problem

distinct 配列で各要素を任意回使い合計 `target` の全組合せ。

### Constraints

- `1 <= candidates.length <= 30`(**distinct**), `2 <= candidates[i] <= 40`, `1 <= target <= 40`

- **同じ候補は何回でも再利用可**。組合せの総数は 150 未満と保証

- 残りの候補リストで順序を固定し、重複組合せを防ぐ。ソート + 大きすぎる候補で終了

### Brute force

- 全列挙、target で剪定。

### Optimized approach + key idea

- **Key idea**: 昇順の候補リストから「先頭を使う / 使わず次へ進む」で分岐する。

  使う枝に同じ候補リストを渡すと再利用でき、左へ戻らないので順序違いの重複が生まれない。

### 個別ファイルとサンプル入力

[Solutions/CombinationSum.hs](Solutions/CombinationSum.hs) に解答本体と実行用 `main` がある。

```sh
nix run path:.#p54
```

```text
入力: combinationSum [2,3,6,7] 7
期待する出力: [[2,2,3],[7]]
```

### Haskell code

```haskell
combinationSum :: [Int] -> Int -> [[Int]]
combinationSum candidates target = go target (sort candidates)
  where
    go 0 _ = [[]]
    go _ [] = []
    go remaining choices@(x : rest)
      | x > remaining = []
      | otherwise = map (x :) (go (remaining - x) choices) ++ go remaining rest
```

### Haskell の読み方

`choices@(x : rest)` は「今使える候補」。

x を使う枝は同じ `choices`、使わない枝は `rest` を渡す。

各結果は不変リストなので path のコピーや復元は不要。

### 解説(なぜ動くか)

- `go remaining choices` は「この候補以降だけを使い、残り remaining を作る組み合わせ」。

- 先頭 x を使う場合は同じ候補リストを渡すため、x を繰り返し使える。使わない場合だけ rest へ進む。

- 候補を左へ戻らないため、[2,3] と [3,2] のような順序違いを生成しない。

- `remaining == 0` の `[[]]` は空の残りを作る 1 通り。候補切れは `[]` = 解なし。この区別が再帰をつなぐ。

- 昇順で x > remaining なら、以降の候補も使えない。候補は正かつ重複なしという制約が必要。

- 不変リストを使うため、可変 path の append/pop やコピーは不要。

### Trace(値の動き)

入力: `combinationSum [2,3] 5`。

| 呼び出し | 分岐 | 返す組み合わせ |
| --- | --- | --- |
| `go 1 [2,3]` | 2 > 1 | `[]` |
| `go 0 [3]` | 残り 0 | `[[]]` |
| `go 3 [3]` | 3 を使い、空の残りに 3 を付ける | `[[3]]` |
| `go 3 [2,3]` | 2 を使う枝は失敗、3 へ進む枝が成功 | `[[3]]` |
| `go 2 [3]` | 3 > 2 | `[]` |
| `go 5 [2,3]` | 2 を使った結果の先頭へ 2 を付ける | `[[2,3]]` |

返り値: `[[2,3]]`。可変の path を使わず、戻り値に `map (x :)` で候補を付ける。

### Complexity

- Time: 指数的(target で枝刈り)。候補のソート O(n log n) と出力の構築も必要。

- Space: O(n + target / minCandidate) 補助空間。返す全組み合わせは別。

### Edge cases

- target = 0 → `[[]]`

- 全 candidate が target より大 → `[]`

### Interview script

<details><summary>全文(読むだけ。暗記しない)</summary>

This problem gives me distinct positive numbers and a target.  
I need all combinations that sum to the target, with reuse allowed and order ignored.

The key idea is backtracking over a sorted list of remaining candidates.  
At each call I either use the first candidate, keeping it available for reuse, or skip it and move to
the rest.
I never move back to a smaller candidate, so I do not generate different orders of the same combination.  
A remaining target of zero returns one empty combination.

An empty candidate list returns no combinations.
If the first candidate is already too large, I stop that branch.  
When a branch uses x, I prepend x to each combination it returns.

For two and three with target five, the successful branch picks two, skips another two, then picks
three.

The answer is two and three.

Time is exponential in the search depth, plus sorting and output construction.  
The maximum depth is the target over the smallest candidate.

Auxiliary space also includes the sorted candidates.

All candidates must be positive, otherwise the reuse branch may fail to make progress.

</details>

### Follow-up

- Q: What if each number can be used only once?

  → A: Combination Sum II; I move start to i plus one, and I skip duplicate values at the same depth.

- Q: How do you stop duplicate combinations?

  → A: The remaining candidate list fixes the order, so each combination is built in one way only.

---

## 25. Koko Eating Bananas [(LC875)](https://leetcode.com/problems/koko-eating-bananas/)

> **Medium** ・ 答え(速さ)を二分探索

**言うこと(暗記はこの 2 行)**

> The key idea is: Binary search the speed. The hours needed only go down as the speed goes up.  
> Time is O of n log of the largest pile.

### Problem

バナナ山を `h` 時間で食べ切る最小速度 `k`。1 時間に最大 1 山。

### Constraints

- `1 <= piles.length <= 10^4`, `1 <= piles[i] <= 10^9`, `piles.length <= h <= 10^9`

- h >= 山の数なので**答えは必ず存在**。1時間に1山だけ・余りは持ち越せない

- 答え(速さ)の二分探索。判定は O(n)、全体 O(n log max)

### Brute force

- k = 1 から線形: O(max_pile * n)。

### Optimized approach + key idea

- **Key idea**: 答え (速度) を二分探索。`feasible(k) = sum(ceil(pile/k)) <= h`。速度に対して単調。

### 個別ファイルとサンプル入力

[Solutions/KokoEatingBananas.hs](Solutions/KokoEatingBananas.hs) に解答本体と実行用 `main` がある。

```sh
nix run path:.#p25
```

```text
入力: minEatingSpeed [3,6,7,11] 8
期待する出力: 4
```

### Haskell code

```haskell
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
```

### Haskell の読み方

切り上げは整数の `(p + speed - 1) div speed`。

合計は `Integer` で計算してオーバーフローを避ける。

速度の範囲は `Int`。

### 解説(なぜ動くか)

- 「答えの二分探索」: 探すのは index ではなく速さ k。

  判定 `feasible(k) = Σ ceil(pile / k) <= h` が k について単調(速いほど時間は減る)なので、false…false
  true…true の境界を二分探索できる。

- feasible(mid) なら `hi = mid`(mid 自身を答え候補として残す)。

  ダメなら `lo = mid + 1`。

  `lo == hi` になった所が最小の true。

- h >= 山の数なので `hi = max(piles)` は必ず feasible → 答えは必ず存在する。

- つまずき: feasible のとき `hi = mid - 1` にすると答えを飛ばす。「true は捨てない」が鉄則。

- つまずき: ceil を切り捨てにすると食べ残しが出る。

  `(p + k - 1) div k`(コードでは div をバッククォートで囲む)。

- 同型: Capacity to Ship Packages, Split Array Largest Sum など「yes/no 判定つき最小化」全般。

### Trace(値の動き)

入力: `piles = [3, 6, 7, 11]`, `h = 8`

| step | lo | hi | mid | 必要時間 Σceil(p/mid) | 動作 |
|---|---|---|---|---|---|
| 1 | 1 | 11 | 6 | 1+1+2+2 = 6 <= 8 | 可 → hi = 6 |
| 2 | 1 | 6 | 3 | 1+2+3+4 = 10 > 8 | 不可 → lo = 4 |
| 3 | 4 | 6 | 5 | 1+2+2+3 = 8 <= 8 | 可 → hi = 5 |
| 4 | 4 | 5 | 4 | 1+2+2+3 = 8 <= 8 | 可 → hi = 4 |
| 5 | 4 | 4 | — | lo == hi | ループ終了 |

返り値: `4`

### Complexity

- Time: O(n log(max_pile))

- Space: O(1)

### Edge cases

- h == n → max(piles)

- piles=[1], h=1 → 1

### Interview script

<details><summary>全文(読むだけ。暗記しない)</summary>

This problem gives me piles of bananas and h hours.  
I need to find the smallest eating speed that finishes all piles in at most h hours.

Let me confirm a few things first.  
Can time left in an hour move to another pile?  
No, each hour is for one pile.  
Is h at least the number of piles?  
Yes, so an answer always exists.

The brute force is to check each speed from one up.  
That is O of n times the max pile time.  
That is too slow.

We can do better.  
The key idea is binary search on the answer: the check "speed k finishes in at most h hours" is
monotone, because a larger speed never needs more hours.

Here is my plan.  
I keep lo at one and hi at the largest pile.  
For each mid speed, I check if the sum of ceiling of pile over mid is at most h.  
If yes, I move hi to mid.  
If no, I move lo to mid plus one.  
I repeat until lo meets hi, and at the end, I return lo.

Let me trace a small example.  
For piles [3, 6, 7, 11] and h eight, speed four needs one, two, two, and three hours, eight in total,
so it works.
Speed three needs ten hours, which is more than eight, so it fails.  
So the smallest valid speed is four.  
That matches.

Time is O of n log of the max pile, because each check is O of n and the speed range drops by half.  
Space is O of one, because I keep only lo and hi.

For edge cases, h equal to the number of piles gives the largest pile, and one pile with one hour gives
that pile's size.

</details>

### Follow-up

- Q: Where else does this pattern apply?

  → A: Any smallest-or-largest question with a yes-no check, like Capacity to Ship Packages or Split
  Array Largest Sum.

- Q: Can you make it faster?

  → A: The bottleneck is the O of n check per step.

  I can start lo at the sum of piles over h, which shrinks the range.

- Q: What if pile sizes are huge?

  → A: Binary search only needs log of the max pile steps, so ten to the ninth is about thirty checks.

---

## 79. Redundant Connection [(LC684)](https://leetcode.com/problems/redundant-connection/)

> **Medium** ・ Union-Find で閉路を作る辺

**言うこと(暗記はこの 2 行)**

> The key idea is: Union find over the edges. The first edge whose two ends already share a root makes the cycle.  
> Time is O of n times alpha of n.

### Problem

木に 1 本余分な辺を加えたグラフで、サイクルを作る冗長辺を返す。

### Constraints

- `3 <= n <= 1000`, **辺数 = n**(木 + 余分な1辺)

- 答えが複数あり得る場合は**入力で最後に現れる辺**を返す

- Union-Find で辺を順に union。最初に閉路を作る辺が答え

### Brute force

- 各辺を外して木か判定: O(n²)。

### Optimized approach + key idea

- **Key idea**: Union-Find。両端が既に同 root の辺が冗長。

### 個別ファイルとサンプル入力

[Solutions/RedundantConnection.hs](Solutions/RedundantConnection.hs) に解答本体と実行用 `main` がある。

```sh
nix run path:.#p79
```

```text
入力: findRedundantConnection [(1,2),(1,3),(2,3)]
期待する出力: Just (2,3)
```

### Haskell code

```haskell
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
```

### Haskell の読み方

親とサイズは `STUArray`。

path compression と union by size の両方を行う。

返り値は閉路を作る辺があれば `Just (a,b)`、なければ `Nothing`。

### 解説(なぜ動くか)

- 木 + 1 辺なので閉路はちょうど 1 つ。閉路を「閉じる」のは入力順で閉路上の最後の辺。

- **不変条件**: parent 配列は処理済みの辺が作る森を表す。

  両端の root が同じ = すでに繋がっている = この辺で閉路完成。

- 入力順に union するから、最初に root が一致した辺がそのまま答え(それ以降の辺を見る必要なし)。

- つまずき: 「答えが複数なら最後の辺を返す」という条件は、順に union するだけで自動的に満たされる。

- path compression と union by size を併用し、find / union は償却 O(α(n))。

### Trace(値の動き)

入力: `edges = [[1,2],[1,3],[2,3]]`(三角形)

| step | edge | parent (node 1..3) | 動作 |
|---|---|---|---|
| 0 | — | `[1,2,3]` | 各ノードが自分の root |
| 1 | [1,2] | `[1,2,3] → [2,2,3]` | root 1 != 2 → union |
| 2 | [1,3] | `→ [2,3,3]` | root(1)=2, root(3)=3 → union |
| 3 | [2,3] | 変化なし | root(2)=3 == root(3)=3 → 閉路を閉じる辺 |

返り値: `[2,3]`

### Complexity

- Time: O(n α(n))

- Space: O(n)

### Edge cases

- サイクルが先頭付近

- 末尾で閉じる

### Interview script

<details><summary>全文(読むだけ。暗記しない)</summary>

This problem gives me a tree plus one extra edge, so the graph has exactly one cycle.  
I need to return the edge that closes the cycle, the last one in input order.

Let me confirm a few things first.  
Is there exactly one extra edge? Yes.  
When more than one answer fits, do I return the last one in the input? Yes.

The brute force is to remove each edge and check if the rest is a tree.  
That is O of n squared time.  
That is too slow.

We can do better.  
The key idea is union-find: when I add the edges in order, the first edge whose two ends already share
a root closes the cycle.

Here is my plan.  
I keep a parent array for union-find.  
I scan the edges one by one.  
For each edge, I check the roots of the two ends.  
If the roots are the same, this edge is the answer, so I return it.  
If not, I merge the two groups.

Let me trace a small example.  
The input is edges [[1, 2], [1, 3], [2, 3]], a triangle.  
The first two edges merge with no problem.  
For [2, 3], both ends already share a root, so I return [2, 3].  
That matches.

Time is O of n alpha of n, because each find is almost O of one with path compression.  
Space is O of n, because of the parent array.

For edge cases, the cycle can close early or at the very last edge, and I return the first closing edge
either way.

</details>

### Follow-up

- Q: What if the graph is directed?

  → A: That is Redundant Connection II.

  I split it into cases: a node with two parents, a cycle, or both.

- Q: What if edges arrive as a stream?

  → A: Union-find fits a stream; I report each edge that closes a cycle when it arrives.

- Q: Can DFS find the edge too?

  → A: Yes, but each check costs O of n, so the scan is O of n squared; union-find is almost linear.

---

## 267. Subarray Sum Equals K [(LC560)](https://leetcode.com/problems/subarray-sum-equals-k/)

> **Medium** ・ prefix の差 = k を Two Sum のように辞書で数える

**言うこと(暗記はこの 2 行)**

> The key idea is: Two Sum on prefix sums. Count earlier prefixes equal to the current prefix minus k.  
> Time is O of n time and space.

### Problem

整数配列 `nums`(**負の数もある**)と整数 `k`。和がちょうど `k` になる連続部分配列の個数を返す。

**仮定**

- 部分配列は連続で、空ではない

- 値は負もありうる(だから sliding window は使えない)

聞くこと: 負の数はあるか / 連続か / 個数か最長か(最長なら辞書の中身が変わる)。

### Constraints

- `1 <= len(nums) <= 2 * 10^4`, `-1000 <= nums[i] <= 1000`, `-10^7 <= k <= 10^7`

- 目標 O(n) 時間、O(n) 空間

### Brute force

- 始点と終点を全部試す: O(n^2)(合計を足しながら伸ばせば O(n^2)、毎回 sum すると O(n^3))。

### Optimized approach + key idea

- **Key idea**: `i+1..j` の和 = `P[j] - P[i]`(P は先頭からの累積和)。

  和が k ⇔ `P[i] = P[j] - k`。

  つまり **prefix に対する Two Sum**。

  左から走査し、「今の prefix − k」が過去に何回出たかを足す。

- 辞書の初期値 `{0: 1}` は「空の prefix」。先頭から始まる部分配列を数えるため。

### 個別ファイルとサンプル入力

[Solutions/SubarraySumEqualsK.hs](Solutions/SubarraySumEqualsK.hs) に解答本体と実行用 `main` がある。

```sh
nix run path:.#p267
```

```text
入力: subarraySum [1,2,-1,2,1] 3
期待する出力: 3
```

### Haskell code

```haskell
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
```

### Haskell の読み方

`hashtables` で平均 O(1) の参照と更新を行う。

`prefix' - k` の件数を読んでから `prefix'` の件数を増やす。

`!` は累積和・答えをその場で評価する。

### 解説(なぜ動くか)

- 保つ不変条件: ループの先頭で `seen[p]` = 「今より前の位置で prefix が p になった回数」(空の prefix
  を含む)。

- 終点 j を固定すると、始点の候補は `P[i] = P[j] - k` を満たす i 全部。

  その個数がちょうど `seen[P[j] - k]` なので、1 回の辞書引きでまとめて数えられる。

- 負の数があると、窓を広げても和が増えるとは限らない。

  だから「和が大きすぎたら左を縮める」sliding window は正しくない。

  全部正なら sliding window で O(1) 空間にできる(追い質問)。

- つまずき 1: **引く → 入れる** の順。

  逆にすると k = 0 のとき、自分自身との差(空の部分配列)を数えてしまう。

  `[0, 0], k = 0` の正解は 3 だが、逆順だと 5 になる。

- つまずき 2: `HT.lookup` の Nothing は `fromMaybe 0` で 0 回として扱う。参照だけではキーを追加しない。

- つまずき 3: 最長を聞かれたら辞書の値は「回数」ではなく「最初の index」。

  初期値は `{0: -1}`、既にあるキーは上書きしない。

### Trace(値の動き)

入力: `nums = [1, 2, -1, 2, 1]`, `k = 3`(初期 `seen = {0: 1}`)

| i | x | prefix | prefix − k | seen[prefix − k] | ans | 入れた後の seen に追加 |
|---|---|---|---|---|---|---|
| 0 | 1 | 1 | −2 | 0 | 0 | 1:1 |
| 1 | 2 | 3 | 0 | 1(空の prefix) | 1 | 3:1 |
| 2 | −1 | 2 | −1 | 0 | 1 | 2:1 |
| 3 | 2 | 4 | 1 | 1 | 2 | 4:1 |
| 4 | 1 | 5 | 2 | 1 | 3 | 5:1 |

返り値: `3`(`[1, 2]`、`[2, -1, 2]`、`[2, 1]`)

### Complexity

- Time: O(n)(1 回の走査、辞書操作は平均 O(1))

- Space: O(n)(異なる prefix の数)

### Edge cases

- k = 0 → 引く → 入れる の順なら空の部分配列は数えない(`[0, 0]` → 3)

- 1 要素で `nums[0] == k` → 1(`{0: 1}` が効く)

- 該当なし → 0。負の k もそのまま動く

### Interview script

<details><summary>全文(読むだけ。暗記しない)</summary>

This problem gives me an array of numbers and a target k.  
I need to return the number of subarrays whose sum is k.

Let me confirm a few things first.  
Can the numbers be negative?  
Yes, so a sliding window does not work.  
Is a subarray contiguous, with no gaps?  
Yes.\

The brute force is to check every start and every end.  
That is O of n squared time.  
That is too slow.

We can do better.  
The key idea is Two Sum on prefix sums.  
The sum from i to j is the prefix at j minus the prefix at i, so I need a past prefix equal to the
current prefix minus k.

Here is my plan.  
I keep a map called seen from a prefix sum to its count, and it starts with zero mapping to one.  
I scan the array from left to right and add each number to the prefix.  
For each prefix, I add the count of prefix minus k to the answer.  
Then I add one to the count of the current prefix.  
At the end, I return the answer.

Let me trace a small example.  
The array is one, two, minus one, two, one, and k is three.  
The prefixes are one, three, two, four, five.  
At three, the map has zero once, so I count one.  
At four, the map has one once, so I count one more.  
At five, the map has two once, so the answer is three.  
That matches.

Time is O of n, because I scan the array once.  
Space is O of n, because of the map.

For edge cases, I look up the map before I add the current prefix, so an empty subarray is never
counted, even when k is zero.

</details>

### Follow-up

- Q: Return the longest subarray with sum k instead.

  → A: The map stores the first index of each prefix, starting with zero mapping to minus one.

  At each index I check prefix minus k and take the longest gap, and I never overwrite an index that is
  already there, because the earliest start gives the longest subarray.

- Q: Count subarrays whose sum is divisible by k.

  → A: Two prefixes differ by a multiple of k exactly when they have the same remainder, so the key
  becomes the prefix modulo k, and I count earlier prefixes with the same remainder.

  In Haskell, use mod with positive k to keep a nonnegative remainder; rem has different sign behavior.

- Q: What if all numbers are positive?

  → A: Then the sum grows when the window grows, so a sliding window works: move the right end, shrink
  the left while the sum is too big, and count when it equals k.

  That is O of one space.

- Q: The same question on a binary tree, paths going down.

  → A: That is Path Sum III.

  I run the same prefix map along the DFS path, and I remove the current prefix from the map when I go
  back up, so only the ancestors are in the map.

- Q: A 2D matrix, count submatrices with sum k.

  → A: I fix a top row and a bottom row, collapse the columns between them into one array of sums, and
  run this same count on it.

  That is O of rows squared times columns.

---

## 268. Increment Submatrices by One (実出題) [(LC2536)](https://leetcode.com/problems/increment-submatrices-by-one/)

> **Medium** ・ 2 次元の差分配列: 矩形の 4 隅に ±1、最後に横と縦の累積和で O(R + n²)
> 出典: 1 件 / 2026-07 の SWE スクリーン(Medium の記事 "Google Interview Question From July 2026"、2026-08-13。レベル不明。練習サイトへ誘導する記事なので信頼度は低め)。差分配列は当番表(P1)のコメントでも "what is it with google and Line Sweep/Difference Arrary" と言われる型

**言うこと(暗記はこの 2 行)**

> The key idea is: A 2D difference array: four corner marks per rectangle, then row and column prefix sums.  
> Time is O of R plus n squared, which is also the lower bound.

### Problem

> [Q] "You are given an integer n describing an n x n grid of zeroes, along with a list of rectangular updates. Each rectangle arrives as [row1, col1, row2, col2] in zero-based inclusive coordinates, and it adds 1 to every cell inside those bounds. Apply every update and return the finished grid"

**仮定**

- 座標は 0 始まりで両端を含む。同じ矩形が何度来ても別々に数える

- 入力は書き換えず、新しい grid を返す。矩形が 0 個なら全部 0

聞くこと: 両端を含むか / n と矩形の数の上限 / 最後にまとめて返すのか、途中で 1
マスずつ聞かれるのか(追い質問)。

### Constraints

- `1 <= n <= 500`、矩形は最大 `10^5` 個(記事の値。LC 2536 は 10^4)

- 素朴に塗ると 10^5 × 250,000 = 2.5 × 10^10。**この掛け算を最初に口で言う**のが評価される

### Brute force

- 矩形ごとに中のマスを全部 +1: O(R n²)。最悪 2.5 × 10^10 で間に合わない。

- 中間: 行ごとに 1 次元の差分(各行で `d[c1] += 1`、`d[c2 + 1] -= 1`)なら O(R n + n²)。

### Optimized approach + key idea

- **Key idea**: 1 次元の差分配列(区間の両端だけに印を付け、最後に累積和で戻す)を 2 次元にする。

  矩形 1 つは 4 隅への ±1 だけで、大きさに関係なく O(1)。

  最後に横の累積和、続けて縦の累積和を取ると各マスの値になる。

- O(R + n²) は下限でもある: 矩形 R 個は読む必要があり、n² マスは書く必要がある。

### 個別ファイルとサンプル入力

[Solutions/IncrementSubmatrices.hs](Solutions/IncrementSubmatrices.hs) に解答本体と実行用 `main` がある。

```sh
nix run path:.#p268
```

```text
入力: incrementSubmatrices 3 [(1,1,2,2),(0,0,1,1)]
期待する出力: [[1,1,0],[1,2,1],[0,1,1]]
```

### Haskell code

```haskell
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
```

### Haskell の読み方

矩形は `(r1,c1,r2,c2)` の 4-tuple。

差分は 2 次元 `STUArray`。

追い質問の `Fenwick2D` は状態を保持する `IO` API で、座標は同じく 0 始まり・両端を含む。

### 解説(なぜ動くか)

- 1 次元: `d[i] += 1`、`d[j + 1] -= 1` の累積和は「i から j までが 1、それ以外は 0」。

- 2 次元: `(r1, c1)` の +1 は、累積和の後「そこから右下全部」を +1 する。

  右の辺の外 `(r1, c2+1)` と下の辺の外 `(r2+1, c1)` で −1 して打ち消すと、右下の外側 `(r2+1, c2+1)`
  から先は 2 回引かれるので、+1 で 1 回戻す(包除原理)。

- 累積和を「横に足す → 縦に足す」の 2 回に分けると、`上 + 左 − 左上` の式を書かずに済み、間違えにくい。

- `(n+1) × (n+1)` で取るのは、右端や下端に触れる矩形で `c2 + 1 = n`、`r2 + 1 = n` に書くため。

  はみ出した行と列は最後に捨てる。

- つまずき: 4 つ目の `+1` を忘れる。右下の外側が −1 になり、矩形の外に負の値が出る。

### Trace(値の動き)

入力: `n = 3`, `rectangles = [[1, 1, 2, 2], [0, 0, 1, 1]]`

```text
marks (4 x 4)         row sums (3 x 3)      column sums = answer
 1  0 -1  0            1  1  0               1  1  0
 0  1  0 -1            0  1  1               1  2  1
-1  0  1  0           -1 -1  0               0  1  1
 0 -1  0  1
```

返り値: `[[1, 1, 0], [1, 2, 1], [0, 1, 1]]`(真ん中は 2 つの矩形が重なるので 2)

### Complexity

- Time: O(R + n²)(印付け R 回、累積和で各マス 2 回)。下限と一致する

- Space: O(n²)

- 追い質問の Fenwick2D: 矩形 1 つも 1 マスの問い合わせも O(log² n)、空間 O(n²)

### Edge cases

- 矩形 0 個 → 全部 0(特別扱いは要らない)

- 右端・下端に触れる矩形 → 余分な 1 行 1 列が受け止める

- 同じ矩形の重複 → そのまま 2 回数える(重複を消すとバグ)

- 1 マスの最大値は矩形の数(10^5)なので 32 bit に収まる

### Interview script

<details><summary>全文(読むだけ。暗記しない)</summary>

This problem gives me a size n and a list of rectangles.  
Each rectangle adds one to every cell inside it, and I need to return the final n by n grid.

Let me confirm a few things first.  
Are the corners inclusive and zero based?  
Yes.  
How large are n and the number of rectangles?  
n is up to five hundred, and there are up to one hundred thousand rectangles.

The brute force is to add one to every cell of every rectangle.  
That is O of R times n squared, about twenty five billion here.  
That is too slow.

We can do better.  
The key idea is a difference array in two dimensions: I mark only the four corners of each rectangle,
and prefix sums at the end rebuild every cell.

Here is my plan.  
I make a difference grid with one extra row and one extra column.  
For each rectangle, I add one at the top left corner, subtract one just right of the right edge and
just below the bottom edge, and add one back past the bottom right corner, because that area was
subtracted twice.
Then I take a running sum along each row, and then along each column.  
At the end, I return the first n rows and columns.

Let me trace a small example.  
n is three, and the rectangles are rows one to two, columns one to two, and rows zero to one, columns
zero to one.
After the sums, the center cell is two, because both rectangles cover it, and the two corners outside
both are zero.
That matches.

Time is O of R plus n squared, because each rectangle is four writes and each cell is summed twice.  
That is also a lower bound, because I must read every rectangle and write every cell.  
Space is O of n squared for the grid.

For edge cases, no rectangles gives all zeros, rectangles on the right or bottom edge write into the
extra row and column, and duplicates are simply counted twice.

</details>

### Follow-up

- Q: Rectangles arrive one at a time, and between them you are asked for the value of one cell.

  → A: The final prefix pass no longer works, so I put the same four corner marks into a two
  dimensional Fenwick tree.

  The value of a cell is the sum of the marks from the top left up to that cell, so an update and a
  query are both O of log squared n.

- Q: After all updates, answer many queries for the sum of a sub-rectangle.

  → A: I build the grid, then a prefix sum grid on top of it.

  Each query is the total up to the bottom right, minus the strip above and the strip to the left, plus
  the top left corner that was removed twice, O of one per query.

- Q: The grid is ten to the ninth wide, but there are few rectangles.

  → A: I compress the coordinates to the rectangle edges, so the grid is at most two R by two R, and
  each compressed cell stands for a block of real cells.

- Q: Each rectangle adds its own value, or you want the cells covered at least k times.

  → A: The corners get plus and minus the value instead of one.

  For at least k, I build the grid and count the cells once, O of n squared.

---

## 270. Minimum Cost to Cut Off All Leaves (実出題)

> **Medium** ・ 木 DP: `cost(v) = Σ min(辺の重み, cost(子))`、葉は ∞。O(n)
> 出典: 1 件 / LC 8527382(L3 インド、2026-09 のオンサイト 2)。追い質問は N 分木と負の重み。重みの持ち方も聞かれた

**言うこと(暗記はこの 2 行)**

> The key idea is: For each child, cut the edge or cut inside the child, whichever is cheaper. A leaf costs infinity.  
> Time is O of n time, O of height space.

### Problem

> [Q] "Given a binary tree with edge weights, remove all leaf nodes from the root. A cut can be made anywhere in the tree, removing the corresponding leaf/subtree. The cost of removing all the leaf nodes is the sum of the weights of the edges being cut. Find the minimum total cost."(LC 8527382)
> [S] 重みを別の配列に添字で持った報告者に、面接官は「もっと良い持ち方は?」と聞き、重みをノードに持たせた Node クラスを書かせた

**仮定**

- 重みは正(報告者が確認)。根は切れない。根だけの木は 0

- 「葉を取り除く」= 根からどの葉にも辿り着けなくする。辺を切るとその下の部分木ごと消える

聞くこと: 重みの符号 / 根だけの木 / 二分木か N 分木か / 重みをどこに持つか。

### Constraints

- `n <= 10^5`、重みは最大 `10^9` 程度(報告に明記なし)。Haskell 版の重みと合計は `Integer`

- 深い木では再帰による stack 使用量に注意。反復の後順走査に直せることを言う

### Brute force

- 切る辺の組み合わせを全部試し、全部の葉が切り離されたものの最小: O(2^(n−1) · n)。

### Optimized approach + key idea

- **Key idea**: `cost(v)` = 「v の下の葉を全部 v から切り離す最小コスト」。

  子 c ごとに、v と c の間の辺を切る(重み w)か、c の中で切る(`cost(c)`)かの安い方。

  子どうしは影響しないので足すだけ: `cost(v) = Σ min(w, cost(c))`。

  葉は自分の中では切れないので `cost = ∞`(親が辺を切るしかない)。

  答えは `cost(根)`。

### 個別ファイルとサンプル入力

[Solutions/MinimumCostToCutOffAllLeaves.hs](Solutions/MinimumCostToCutOffAllLeaves.hs) に解答本体と実行用 `main` がある。

```sh
nix run path:.#p270
```

```text
入力: root -> A (4), B (7); A -> leaves (2,3); B -> leaf (1)
期待する出力: 5
```

### Haskell code

```haskell
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
```

### Haskell の読み方

重みと合計は `Integer`。

葉の「内部で切れない」は `Nothing` (= ∞) で表し、`maybe w (min w)` で辺との最小を取る。

根だけの場合は 0。

### 解説(なぜ動くか)

- 子 c の部分木の葉は、c を通らないと根に届かない。

  だから「辺 (v, c) を切る」か「c の中で全部切る」のどちらかが必要十分で、子どうしは影響しない。

  これが `Σ min(w, cost(c))` の理由。

- 葉の ∞ は「入ってくる辺を切る以外に方法がない」を表す。

  ∞ は `min` で必ず w に負けるので、葉の親では辺の重みがそのまま足される。

- 重みの持ち方: 親が `(子, 重み)` を組で持つと、辺を 1 回ずつ見るだけで済み、N 分木にもそのまま広がる。

  別の配列に添字で持つと、ノードと添字の対応がずれるバグが出やすい(面接官の指摘はこれ)。

- 負の重みは意味を聞く。

  (a) 切る辺の集合を選ぶ(切った下の辺も切れる)なら、負の辺は全部取って得なので
  `minCutTakeNegatives`(報告者の答え)。

  (b) 消えた部分木の中はもう切れないなら、基本の `Σ min(w, cost(c))` がそのまま正しい(`min`
  が負の辺も下の安い切り方も選ぶ)。

  どちらの意味なのかを最初に確認する。

- つまずき: 根を葉と同じに扱って ∞ を返すと、根だけの木で答えが ∞ になる。根は別扱い。

- 負の重み版は `(負の辺の合計, 非負の追加コスト)` を組で返す。

  共有変数の更新順に依存せず、切った下にある負の辺も必ず調べる。

### Trace(値の動き)

```text
             root
         4 /      \ 7
          A        B
       2 / \ 3      \ 1
       L1   L2       L3
```

| 節点 | 計算 | cost |
|---|---|---|
| L1, L2, L3 | 葉 | ∞ |
| A | min(2, ∞) + min(3, ∞) | 5 |
| B | min(1, ∞) | 1 |
| root | min(4, cost(A) = 5) + min(7, cost(B) = 1) | **5** |

返り値: `5`(A の上の辺 4 と、L3 の上の辺 1 を切る)

### Complexity

- Time: O(n)(各節点と各辺を 1 回)

- Space: O(h)(再帰の深さ)

### Edge cases

- 根だけ → 0。根の子が全部葉 → 根から出る辺の合計

- 一本道(根 − a − b − 葉)→ 道の上で最小の辺

- 深い木 → 反復の後順走査に直す

### Interview script

<details><summary>全文(読むだけ。暗記しない)</summary>

This problem gives me a binary tree with a weight on each edge.  
I need the minimum total weight of edges to cut so that no leaf is still connected to the root.

Let me confirm a few things first.  
Are the weights positive?  
Yes.  
If the root has no children, is the answer zero?  
Yes, the root itself is never cut.

The brute force is to try every set of edges and keep the cheapest set that cuts off all leaves.  
That is O of two to the n time.  
That is too slow.

We can do better.  
The key idea is that each child is independent: for each child, I either cut the edge to it or cut
everything inside it, whichever is cheaper.

Here is my plan.  
I store the weight of each edge in the parent node, next to the child it leads to.  
I write a recursive function called cost that returns the cheapest way to cut off all leaves below a
node.
A leaf returns Nothing to mean infinity, because only its parent can cut it off.  
For any other node, I add the cheaper cost for each child: its edge weight, or its inside cost.

Nothing forces the edge to be cut.
At the end, I return the cost of the root.

Let me trace a small example.  
The root has child A with weight four and child B with weight seven.  
A has two leaves with weights two and three, so cutting inside A costs five, and cutting the edge costs
four.
B has one leaf with weight one, so cutting inside B costs one.  
The answer is four plus one, five.  
That matches.

Time is O of n, because I visit each node and each edge once.  
Space is O of the height, because of the recursion.

For edge cases, a root with no children returns zero, and for a very deep tree I would switch to an
iterative post order.

</details>

### Follow-up

- Q: A node can have any number of children.

  → A: Each node keeps a list of pairs, the child and the edge weight.

  The cost is the same sum of min over the list, so the code barely changes.

- Q: Edge weights can be negative.

  → A: First I ask what a cut means.

  If I choose any set of edges, I always cut every negative edge, because it lowers the cost, and then
  cut off the remaining leaves with positive edges at the minimum cost.

  If nothing can be cut inside a removed subtree, the same sum of min is still correct, because the min
  picks the negative edge or the cheaper cuts below it.

- Q: Return which edges to cut.

  → A: At each child I record whether the edge or the inside was cheaper, then walk down from the root
  and collect the edges marked as cut.

- Q: The tree has a million nodes in a chain.

  → A: Recursion would overflow, so I use an iterative post order with a stack, computing a node after
  both of its children.

---
