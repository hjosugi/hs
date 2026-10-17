module Common.Example (example) where

import Control.Monad (unless)

-- サンプル入力・出力を表示し、期待値との一致も確認する。
example :: (Eq a, Show a) => String -> a -> a -> IO ()
example input expected actual = do
  putStrLn ("入力: " ++ input)
  putStrLn ("期待する出力: " ++ show expected)
  putStrLn ("実際の出力: " ++ show actual)
  unless (actual == expected) $ fail "サンプルの出力が期待値と一致しません"
