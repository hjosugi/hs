# Haskell 版 Must 50

Nix があれば、GHC やライブラリの個別インストールなしで動く。GHC・Cabal・`hashtables` を `flake.lock` で固定する。

リポジトリ直下から:

```sh
cd hs
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

単独の練習ファイルも、この環境で `runghc practice.hs` と実行できる。解法を呼ぶ場合は `import Must50DCore` を書く。

Nix の環境を含めて検証するには `nix flake check path:.`。`nix-command` / `flakes` が無効な環境では、各 Nix コマンドを `nix --extra-experimental-features 'nix-command flakes' …` と実行する。

- [全問の解説](must50-d-core.md)
- [解法モジュール](Must50DCore.hs)
- [実行サンプル](app/Main.hs)
- [全問テスト](test/Check.hs)
