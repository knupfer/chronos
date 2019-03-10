module Main where

import Chronos.Bench

main :: IO ()
main = defaultMain $ map (\n -> bench ("fib " ++ show n) fib n) [1..10]

fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib (n-2)
