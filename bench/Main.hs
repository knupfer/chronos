module Main where

import Chronos

main :: IO ()
main = defaultMain $
  [ bench "id ()" id ()
  , benchIO "pure ()" (pure ())
  , bench "succ 1" succ (1::Int)
  , bench "not True" not True
  , bench "reverse \"abc\"" reverse "abc"
  ] ++ map (\n -> bench ("fib " ++ show n) fib n) [1..8]

fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib (n-2)
