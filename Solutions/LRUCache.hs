module Solutions.LRUCache where

import Common.Example (example)
import Control.Monad (forM_, when)
import qualified Data.HashTable.IO as H
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)

-- 問 34: 解答本体。main は末尾のサンプル入力で実行する。
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
  maybe
    (writeIORef (cacheHead cache) next)
    (\p -> writeIORef (cacheNext p) next)
    previous
  maybe
    (writeIORef (cacheTail cache) previous)
    (\q -> writeIORef (cachePrev q) previous)
    next

addFront :: LRUCache -> CacheNode -> IO ()
addFront cache node = do
  first <- readIORef (cacheHead cache)
  writeIORef (cachePrev node) Nothing
  writeIORef (cacheNext node) first
  maybe
    (writeIORef (cacheTail cache) (Just node))
    (\p -> writeIORef (cachePrev p) (Just node))
    first
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

main :: IO ()
main = do
  cache <- newLRUCache 2
  lruPut cache 1 1
  lruPut cache 2 2
  first <- lruGet cache 1
  lruPut cache 3 3
  missing <- lruGet cache 2
  example "capacity=2; put(1,1); put(2,2); get(1); put(3,3); get(2)" [1, -1] [first, missing]
