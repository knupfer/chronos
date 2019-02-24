module Main where

import Chronos
import Parser

import Control.Applicative
import Options.Applicative

type Arguments = (Config, [String])

main :: IO ()
main = uncurry defaultMainWith . fmap (map (benchShell <*> id)) =<< execParser opts
  where
    opts = info (arguments <**> helper)
      ( fullDesc <> header "chronos - a tool to comparatively benchmark programs with lazy precision" )

arguments :: Parser Arguments
arguments
  = liftA2 (,) (configParser Config)
  $ liftA2 (:)
  ( argument str (metavar "COMMAND"))
  ( many (argument str (metavar "COMMAND")))
