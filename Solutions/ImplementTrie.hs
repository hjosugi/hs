module Solutions.ImplementTrie where

import Common.Example (example)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)

-- 問 108: 解答本体。main は末尾のサンプル入力で実行する。
-- BEGIN 108
data Trie = Trie
  { isWord :: !Bool
  , trieChildren :: M.Map Char Trie
  }
  deriving (Show)

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

main :: IO ()
main = do
  let trie = trieInsert "app" (trieInsert "apple" emptyTrie)
  example
    "insert \"apple\"; insert \"app\"; search \"app\"; search \"ap\"; startsWith \"ap\""
    [True, False, True]
    [trieSearch "app" trie, trieSearch "ap" trie, trieStartsWith "ap" trie]
