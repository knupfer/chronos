module Main where

import Chronos

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
  = liftA2 (,) config
  $ liftA2 (:)
  ( argument str (metavar "COMMAND"))
  ( many (argument str (metavar "COMMAND")))

config :: Parser Config
config = Config
  <$> switch ( long "hide-bar" <> help "Hide the bar indicating relative performance." )
  <*> switch ( long "same-line" <> help "Print the analysis on the same line as the command." )
  <*> switch ( long "hide-details" <> help "Hide standard deviation and number of samples." )
  <*> switch ( long "print-once" <> help "Print only once the analysis.  This is will print the analysis on timeout, maximal relative error or ctrl-c." )
  <*> switch ( long "sort" <> help "Sort benchmarks by mean duration." )
  <*> switch ( long "simple" <> help "Don't colorize output and don't use unicode." )
  <*> option auto
  ( long "confidence"
    <> help "Factor by which the standard error will be multiplied for calculating confidence intervals (default is 6)."
    <> value 6
    <> metavar "DOUBLE"
  )
  <*> optional
  ( option auto
    ( long "timeout"
      <> help "Timeout after which the program is terminated. It finishes the currently running benchmark."
      <> metavar "DOUBLE"
    )
  )
  <*> optional
  ( option auto
    ( long "relative-error"
      <> help "After every benchmark has got a relative error (calculated via confidence interval) below DOUBLE the program is terminated."
      <> metavar "DOUBLE"
    )
  )
