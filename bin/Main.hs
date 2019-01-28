{-# LANGUAGE DataKinds #-}

module Main where

import Chronos

import System.Environment
import System.Process
import Control.Monad

main :: IO ()
main = defaultMain . map (bench <*> nfIO run) =<< getArgs

  where run x = withCreateProcess (shell x) {std_out = CreatePipe, std_err = CreatePipe} $ \_ _ _ ph ->
                void $ waitForProcess ph
