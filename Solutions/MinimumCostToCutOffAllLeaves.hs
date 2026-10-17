{-# LANGUAGE BangPatterns #-}

module Solutions.MinimumCostToCutOffAllLeaves where

import Common.Example (example)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Prelude hiding (foldl')

-- 問 270: 解答本体。main は末尾のサンプル入力で実行する。
-- BEGIN 270
-- Nothing を「内部では切れない = ∞」として使う。浮動小数点は不要。
data CutNode = CutNode
  { leftChild :: Maybe (Integer, CutNode)
  , rightChild :: Maybe (Integer, CutNode)
  }
  deriving (Show)

minCutLeaves :: Maybe CutNode -> Integer
minCutLeaves Nothing = 0
minCutLeaves (Just root) = fromMaybe 0 (cost root)
 where
  cost (CutNode Nothing Nothing) = Nothing
  cost (CutNode left right) =
    Just $
      foldl'
        (+)
        0
        [maybe w (min w) (cost child) | Just (w, child) <- [left, right]]

data NaryNode = NaryNode [(Integer, NaryNode)]
  deriving (Show)

minCutNary :: NaryNode -> Integer
minCutNary = fromMaybe 0 . cost
 where
  cost (NaryNode []) = Nothing
  cost (NaryNode children) =
    Just $
      foldl'
        ( \total (w, child) ->
            total + maybe w (min w) (cost child)
        )
        0
        children

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

main :: IO ()
main = do
  let leaf = CutNode Nothing Nothing
      left = CutNode (Just (2, leaf)) (Just (3, leaf))
      right = CutNode (Just (1, leaf)) Nothing
      root = CutNode (Just (4, left)) (Just (7, right))
  example "root -> A (4), B (7); A -> leaves (2,3); B -> leaf (1)" 5 (minCutLeaves (Just root))
