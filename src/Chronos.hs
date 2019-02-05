{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Chronos where

import Numeric
import Numeric.Natural
import Data.Char
import Data.List
import System.IO
import Data.Time.Clock.System
import Control.Monad
import System.Console.ANSI
import Control.Exception
import Control.DeepSeq
import System.Process

type Name = String

data Benchmark
  = Benchmark
  { name :: Name
  , runner :: (Analysis -> IO Analysis)
  , analysis :: Analysis
  }

data Analysis
  = Analysis
  { samples :: !Natural
  , squaredWeights :: !Natural
  , mean :: !Rational
  , qFactor :: !Rational
  , stdError :: !Double
  , information :: !Double
  }

variance :: Analysis -> Rational
variance Analysis{..}
  | samples <= 1 = 0
  | otherwise = qFactor / fromIntegral (samples - 1)

sigma :: Analysis -> Double
sigma = sqrt . fromRational . variance

stdError' :: Analysis -> Double
stdError' Analysis{..}
  | samples > 1 = sqrt (fromRational (fromIntegral squaredWeights * qFactor / fromIntegral (samples - 1))) / fromIntegral samples
  | otherwise = 0

step :: Benchmark -> IO Benchmark
step (Benchmark n f a) = Benchmark n f <$> f a

benchIO :: Name -> IO a -> Benchmark
benchIO n io = Benchmark n f (Analysis 0 0 0 0 0 0)

  where
    f Analysis{..} = do
      let weight | mean == 0 = 1
                 | otherwise = max 1 (min samples . round $ 0.1 / mean)
      time <- runBench weight
      let newSamples = samples + weight
          newMean = mean + fromIntegral weight * (time - mean) / fromIntegral newSamples
          newQFactor = qFactor + fromIntegral weight * (time - mean) * (time - newMean)
          newSquaredWeights = squaredWeights + weight*weight
          new = Analysis newSamples newSquaredWeights newMean newQFactor 0 0
          newStdError = stdError' new
          newInformation = recip $ 1/ 2^newSamples + (newStdError / fromRational newMean) / sqrt (fromIntegral newSamples)

      return $ new {stdError = newStdError, information = newInformation}

    runBench weight = do
            begin <- getSystemTime
            replicateM_ (fromIntegral weight) $ io
            end <- getSystemTime
            return $ (toSeconds end - toSeconds begin) / fromIntegral weight
    toSeconds t = fromIntegral (systemSeconds t) + fromIntegral (systemNanoseconds t) / 1000000000

benchShell :: Name -> String -> Benchmark
benchShell n cmd = benchIO n $ withCreateProcess (shell cmd) {std_out = CreatePipe, std_err = CreatePipe} $ \_ _ _ ph ->
  void $ waitForProcess ph

bench :: NFData b => Name -> (a -> b) -> a -> Benchmark
bench n f = benchIO n . evaluate . force . f

defaultMain :: [Benchmark] -> IO ()
defaultMain bs = bracket_ hideCursor showCursor $ do
  bs' <- forM bs $ \b -> do
    printName (name b)
    printIndicator
    b' <- step b
    printAnalysis (analysis b')
    putStrLn ""
    return b'

  runMain . zip [1..] $ bs'

runMain :: [(Int, Benchmark)] -> IO ()
runMain xs = do
  replicateM_ 3 $ putStrLn ""
  go (1000 :: Int) xs
  where
    go 0 _ = return ()
    go n zs = go (n-1) . increasePrecision =<< runHead zs

    runHead [] = pure []
    runHead ((n,u):us) = let mv = (length xs - n + 1) * 3
                         in bracket_ (cursorUpLine (mv+2)) (cursorDownLine mv) $ do
      printIndicator
      u' <- step u
      printAnalysis (analysis u')
      printBar (Percentage . fromRational $ mean (analysis u') / maximum (map (mean . analysis . snd) $ (n,u'):us))
               (Percentage $ sigma (analysis u') / fromRational (mean (analysis u')))
      pure ((n,u'):us)

    increasePrecision = sortOn (information . analysis . snd)

printIndicator :: IO ()
printIndicator = do
  setSGR [SetColor Foreground Vivid Red]
  putStr "►"
  setSGR [Reset]
  hFlush stdout

printAnalysis :: Analysis -> IO ()
printAnalysis ana = do
  clearLine
  putStrLn (' ':show ana)

newtype Percentage = Percentage Double deriving (RealFrac, Real, Num, Fractional, Ord, Eq)

printBar :: Percentage -> Percentage -> IO ()
printBar m sd = do
  clearLine
  putStrLn (' ':' ':replicate (round len) '█' ++ take 40 (replicate (round (len * sd)) '─'))
  where len = m * 60

printName :: Name -> IO ()
printName n = do
  clearLine
  setSGR [SetColor Foreground Vivid Cyan]
  putStrLn n
  clearLine
  setSGR [Reset]

instance Show Analysis where
  show a@Analysis{..}
    = concat
    [ "x=", prettyScientific (fromRational mean) (Just stdError) "s "
    , "σ=", prettyScientific (100*sigma a/fromRational mean) Nothing "% "
    , "n=", show samples
    ]

ord0 :: Int
ord0 = ord '0'

ordDot :: Int
ordDot = ord '.'

prettyScientific :: Double -> Maybe Double -> String -> String
prettyScientific x b unit = concat $ case floatToDigits 10 <$> b of
    Nothing -> [mantissa (take 2 $ sig ++ repeat 0), f expo, unit]
    Just (errSig,errExpo) -> [mantissa (take (max 2 $ valLen errExpo) $ sig ++ repeat 0), showError errSig, f expo, unit]
  where

    showError err = '(' : map (chr . (ord0+)) (take 2 $ err ++ repeat 0) ++ ")"
    (sig,expo) = floatToDigits 10 x
    valLen e = expo - e + 2
    mantissa [d] = [chr $ ord0+d]
    mantissa (d:ds) = map (chr . (ord0+)) (d:ordDot-ord0:ds)
    mantissa [] = ""
    f 1 = ""
    f 2 = "·10"
    f e = "·10" ++ showE (e-1)

showE :: Integral a => a -> String
showE = \case
  0 -> "⁰"
  1 -> "¹"
  2 -> "²"
  3 -> "³"
  4 -> "⁴"
  5 -> "⁵"
  6 -> "⁶"
  7 -> "⁷"
  8 -> "⁸"
  9 -> "⁹"
  n | n < 0     -> '⁻' : showE (negate n)
    | otherwise -> (\(a, b) -> showE a ++ showE b) $ divMod n 10
