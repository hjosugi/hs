module Solutions.StatusMessageDeduplication where

import Common.Example (example)
import Control.Monad (forM_, when)
import qualified Data.HashTable.IO as H
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Sequence as Q

-- 問 260: 解答本体。main は末尾のサンプル入力で実行する。
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
                Just (PendingMessage _ _ latestFlag)
                  | latestFlag == flag ->
                      H.delete (suppressLatest deduper) message
                _ -> pure ()
              loop rest (if suppressed then output else (timestamp, message) : output)
        _ -> do
          writeIORef (suppressPending deduper) queue
          pure (reverse output)
  loop pending []

-- END 260

main :: IO ()
main = do
  deduper <- newMessageDeduper 10 True
  results <- mapM (\t -> shouldPrint deduper t "solar") [10, 13, 20, 21, 31, 35]
  example
    "window=10, strict=True, message=\"solar\", timestamps=[10,13,20,21,31,35]"
    [True, False, False, True, False, True]
    results
