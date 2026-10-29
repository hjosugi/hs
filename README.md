# Haskell 版 Must 50

Nix があれば、GHC やライブラリの個別インストールなしで動く。

GHC・Cabal・`hashtables`・HLS・Fourmolu を `flake.lock` で固定する。

このリポジトリの直下で:

```sh
nix run path:.
```

サンプルの結果が表示される。初回は Nix が必要なパッケージを取得する。

```text
Must 50 Haskell examples
61  Merge Intervals: [(1,6),(8,10)]
68  Number of Islands: 2
93  Word Break: True
86  House Robber: 12
94  Longest Increasing Subsequence: 4
52  Median: Just 2.0
34  LRU get(1): 10
267 Subarray Sum Equals K: 3
```

## 1 問ずつ実行する

各ファイルに解答本体と `main` がある。

実行すると入力・期待する出力・実際の出力が表示され、結果が違えばエラーになる。

```sh
nix run path:.#p61
nix run path:.#p68
nix run path:.#p93
```

全問の配置:

| 問 | 解答ファイル | 実行 |
| --- | --- | --- |
| 61 | [MergeIntervals.hs](Solutions/MergeIntervals.hs) | `nix run path:.#p61` |
| 64 | [MeetingRoomsII.hs](Solutions/MeetingRoomsII.hs) | `nix run path:.#p64` |
| 68 | [NumberOfIslands.hs](Solutions/NumberOfIslands.hs) | `nix run path:.#p68` |
| 260 | [StatusMessageDeduplication.hs](Solutions/StatusMessageDeduplication.hs) | `nix run path:.#p260` |
| 75 | [CourseSchedule.hs](Solutions/CourseSchedule.hs) | `nix run path:.#p75` |
| 80 | [WordLadder.hs](Solutions/WordLadder.hs) | `nix run path:.#p80` |
| 82 | [NetworkDelayTime.hs](Solutions/NetworkDelayTime.hs) | `nix run path:.#p82` |
| 34 | [LRUCache.hs](Solutions/LRUCache.hs) | `nix run path:.#p34` |
| 5 | [TopKFrequentElements.hs](Solutions/TopKFrequentElements.hs) | `nix run path:.#p5` |
| 52 | [MedianFinder.hs](Solutions/MedianFinder.hs) | `nix run path:.#p52` |
| 14 | [LongestSubstringWithoutRepeatingCharacters.hs](Solutions/LongestSubstringWithoutRepeatingCharacters.hs) | `nix run path:.#p14` |
| 108 | [ImplementTrie.hs](Solutions/ImplementTrie.hs) | `nix run path:.#p108` |
| 111 | [WordSearchII.hs](Solutions/WordSearchII.hs) | `nix run path:.#p111` |
| 101 | [SlidingWindowMaximum.hs](Solutions/SlidingWindowMaximum.hs) | `nix run path:.#p101` |
| 93 | [WordBreak.hs](Solutions/WordBreak.hs) | `nix run path:.#p93` |
| 86 | [HouseRobber.hs](Solutions/HouseRobber.hs) | `nix run path:.#p86` |
| 95 | [PartitionEqualSubsetSum.hs](Solutions/PartitionEqualSubsetSum.hs) | `nix run path:.#p95` |
| 94 | [LongestIncreasingSubsequence.hs](Solutions/LongestIncreasingSubsequence.hs) | `nix run path:.#p94` |
| 45 | [BinaryTreeMaximumPathSum.hs](Solutions/BinaryTreeMaximumPathSum.hs) | `nix run path:.#p45` |
| 21 | [DailyTemperatures.hs](Solutions/DailyTemperatures.hs) | `nix run path:.#p21` |
| 54 | [CombinationSum.hs](Solutions/CombinationSum.hs) | `nix run path:.#p54` |
| 25 | [KokoEatingBananas.hs](Solutions/KokoEatingBananas.hs) | `nix run path:.#p25` |
| 79 | [RedundantConnection.hs](Solutions/RedundantConnection.hs) | `nix run path:.#p79` |
| 267 | [SubarraySumEqualsK.hs](Solutions/SubarraySumEqualsK.hs) | `nix run path:.#p267` |
| 268 | [IncrementSubmatrices.hs](Solutions/IncrementSubmatrices.hs) | `nix run path:.#p268` |
| 270 | [MinimumCostToCutOffAllLeaves.hs](Solutions/MinimumCostToCutOffAllLeaves.hs) | `nix run path:.#p270` |

## GHCi

GHCi で各問の関数を試す:

```sh
nix run path:.#repl
```

```haskell
mergeIntervals [(1,3),(2,6),(8,10)]
wordBreak "leetcode" ["leet","code"]
combinationSum [2,3,6,7] 7
:quit
```

全 25 問・補足 260・追い質問をテストする:

```sh
nix run path:.#test
```

編集しながら使う場合は開発環境に入る。依存は Nix が供給するため `cabal update` は不要。

```sh
nix develop path:.
cabal run must50-hs
cabal repl lib:must50-hs
cabal test
```

個別ファイルを直接実行する場合:

```sh
runghc Solutions/MergeIntervals.hs
runghc Solutions/NumberOfIslands.hs
runghc Solutions/WordBreak.hs
```

`Must50DCore.hs` は全問をまとめて使うための再エクスポート。

解法本体は `Solutions/`、共通の heap や配列の補助は `Common/` に置いてある。

## VS Code の Haskell 設定

`code .` で、このリポジトリをフォルダーとして開く。

- `.vscode/settings.json`: HLS の起動と、Fourmolu による保存時の整形。

- `bin/hls`: Nix の同じ GHC で HLS を起動する。VS Code 自体は通常どおり開ける。

- `hie.yaml`: ライブラリ・サンプル実行・テストを正しい Cabal component に対応付ける。

- `fourmolu.yaml`: 2 スペース・100 桁で整形する。

- `Haskell: run current file` タスク: 開いている問題ファイルのサンプルを実行する。

Haskell 拡張は `.vscode/extensions.json` に指定。

補完、型情報、エラー表示、整形に使うツールは Nix で固定している。

Nix の環境を含めて検証するには `nix flake check path:.`。

`nix-command` / `flakes` が無効な環境では、各 Nix コマンドを
`nix --extra-experimental-features 'nix-command flakes' …` と実行する。

- [全問の解説](must50-d-core.md)

- [解法モジュール](Must50DCore.hs)

- [実行サンプル](app/Main.hs)

- [全問テスト](test/Check.hs)
