module Solutions.WordSearchII where

import Common.Arrays (newBoard)
import Common.Example (example)
import Control.Monad (foldM, when)
import Control.Monad.ST (runST)
import Data.Array.MArray (readArray, writeArray)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.STRef (modifySTRef', newSTRef, readSTRef)
import Solutions.ImplementTrie (Trie (..), emptyTrie, trieInsert)
import Prelude hiding (foldl')

-- 問 111: 解答本体。main は末尾のサンプル入力で実行する。
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
                let neighbors =
                      [ (r', c')
                      | (r', c') <- [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)]
                      , 0 <= r'
                      , r' < m
                      , 0 <= c'
                      , c' < n
                      ]
                child <- foldM (dfs path') (Trie False next) neighbors
                writeArray board (r, c) ch
                let children' =
                      if not (isWord child) && M.null (trieChildren child)
                        then M.delete ch children
                        else M.insert ch child children
                pure (Trie terminal children')
      _ <- foldM (dfs []) root cells
      reverse <$> readSTRef found

-- END 111

main :: IO ()
main =
  example
    "findWords [\"ab\",\"cd\"] [\"ab\",\"acd\"]"
    (["acd", "ab"])
    (findWords ["ab", "cd"] ["ab", "acd"])
