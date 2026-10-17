module Common.Arrays where

import Control.Monad.ST (ST)
import Data.Array.MArray (newArray, newListArray)
import Data.Array.ST (STUArray)

newInts :: (Int, Int) -> Int -> ST s (STUArray s Int Int)
newInts = newArray

newBools :: (Int, Int) -> Bool -> ST s (STUArray s Int Bool)
newBools = newArray

newGrid :: ((Int, Int), (Int, Int)) -> Int -> ST s (STUArray s (Int, Int) Int)
newGrid = newArray

newBoard :: ((Int, Int), (Int, Int)) -> [Char] -> ST s (STUArray s (Int, Int) Char)
newBoard = newListArray
