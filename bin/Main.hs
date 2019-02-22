module Main where

import Chronos
import System.Environment

main :: IO ()
main = defaultMain . map (benchShell <*> id) =<< getArgs
