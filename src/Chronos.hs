{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Chronos where

import Data.Function
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
  , runner :: Analysis -> IO Analysis
  , analysis :: Analysis
  }

data Analysis
  = Analysis
  { samples :: Natural
  , squaredWeights :: Natural
  , mean :: Rational
  , qFactor :: Rational
  , stdError :: Double
  , information :: Double
  }

variance :: Analysis -> Rational
variance Analysis{..}
  | samples <= 1 = 0
  | otherwise = qFactor / fromIntegral (samples - 1)

sigma :: Analysis -> Double
sigma = sqrt . fromRational . variance

stdError' :: Analysis -> Double
stdError' Analysis{..}
  | samples > 1 = sqrt $ fromRational (fromIntegral squaredWeights * qFactor / fromIntegral (samples - 1)) / fromIntegral samples
  | otherwise = 0

step :: Benchmark -> IO Benchmark
step (Benchmark n f a) = Benchmark n f <$> f a

benchIO :: Name -> IO a -> Benchmark
benchIO n io = Benchmark n f (Analysis 0 0 0 0 0 0)

  where
    f Analysis{..} = do
      let weight | mean == 0 = 1
                 | otherwise = max 1 (min (samples `div` 2) . round $ 0.2 / mean)
      time <- runBench weight
      let newSamples = samples + weight
          newMean = mean + fromIntegral weight * (time - mean) / fromIntegral newSamples
          newQFactor = qFactor + fromIntegral weight * (time - mean) * (time - newMean)
          newSquaredWeights = squaredWeights + weight*weight
          new = Analysis newSamples newSquaredWeights newMean newQFactor 0 0
          newStdError = stdError' new
          newInformation = recip $ 1/ 1.5^newSamples + (newStdError / fromRational newMean) / sqrt (fromIntegral newSamples)

      return $ new {stdError = newStdError, information = newInformation}

    runBench weight = do
            begin <- getSystemTime
            replicateM_ (fromIntegral weight) io
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

  putStrLn ""
  runMain (length bs') . zip [1..] $ bs'

runMain :: Int -> [(Int, Benchmark)] -> IO ()
runMain len = fix (go>=>)
  where
    go = fmap increasePrecision . runHead

    runHead [] = pure []
    runHead ((n,u):us) = let mv = (len - n) * 3 + 1
                         in bracket_ (cursorUpLine (mv+2)) (cursorDownLine mv) $ do
      printIndicator
      u' <- step u
      printAnalysis (analysis u')
      printBar (Percentage . fromRational $ mean (analysis u') / maximum (map (mean . analysis . snd) $ (n,u'):us))
               (Percentage $ sigma (analysis u') / fromRational (mean $ analysis u'))
      pure ((n,u'):us)

    increasePrecision = sortOn (information . analysis . snd)

printIndicator :: IO ()
printIndicator = do
  setSGR [SetColor Foreground Vivid Red]
  putChar '►'
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
    , "σ=", prettyScientific (100*sigma a/ fromRational mean) Nothing "% "
    , "n=", showNumWithSpaces samples
    ]

showNumWithSpaces :: (Show a) => a -> String
showNumWithSpaces x = reverse . intercalate "," . go $ reverse (show x)
  where go ds | length ds > 3 = let (a,b) = splitAt 3 ds in a : go b
              | otherwise = [ds]

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
    f e | e < 1 = "·10⁻" ++ map showE (digitList $ abs (e-1))
        | otherwise = "·10" ++ map showE (digitList $ e-1)

digitList :: Integral a => a -> [a]
digitList x = case divMod x 10 of
   (0, b) -> [b]
   (a, b) -> digitList a ++ [b]

showE :: Integral a => a -> Char
showE = \case
  0 -> '⁰'
  1 -> '¹'
  2 -> '²'
  3 -> '³'
  4 -> '⁴'
  5 -> '⁵'
  6 -> '⁶'
  7 -> '⁷'
  8 -> '⁸'
  9 -> '⁹'
  _ -> error "Only for single digts"
