{-# LANGUAGE BangPatterns #-}

module Solutions.LongestSubstringWithoutRepeatingCharacters where

import Common.Example (example)
import Control.Monad.ST (runST)
import qualified Data.HashTable.ST.Basic as HT

-- 問 14: 解答本体。main は末尾のサンプル入力で実行する。
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

main :: IO ()
main =
  example
    "lengthOfLongestSubstring \"abcabcbb\""
    (3)
    (lengthOfLongestSubstring "abcabcbb")
