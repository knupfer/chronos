{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Chronos
  ( Benchmark(..)
  , Analysis(..)
  , defaultMain
  , bench
  , benchIO
  , benchShell
  , sigma
  , stdError
  , step
  )
  where

import Data.String
import Control.Arrow
import Data.Function
import Numeric
import Data.List
import System.IO
import Data.Time.Clock.System
import Control.Monad
import System.Console.ANSI
import System.Console.ANSI.Codes
import Control.Exception
import Control.DeepSeq
import System.Process
import System.Console.Terminal.Size

import qualified Data.Set as S
import qualified Data.ByteString.Builder as B

data Benchmark
  = Benchmark
  { name :: String
  , runner :: Analysis -> IO Analysis
  , analysis :: Analysis
  }

data BenchmarkMeta
  = BenchmarkMeta
  { information :: Double
  , position :: Int
  , benchmark :: Benchmark
  }

instance Eq BenchmarkMeta where
  (==) = (==) `on` position &&& analysis . benchmark

instance Ord BenchmarkMeta where
  compare = compare `on` information &&& position &&& analysis . benchmark

data Analysis
  = Analysis
  { samples :: Word
  , squaredWeights :: Word
  , mean :: Rational
  , qFactor :: Rational
  , variance :: Rational
  } deriving (Eq, Ord, Show, Read)

defaultMain :: [Benchmark] -> IO ()
defaultMain bs = bracket_ hideCursor showCursor $
  B.hPutBuilder stdout (foldMap (nameBuilder . name) bs <> B.char7 '\n')
  *> runMain (S.fromList $ zipWith (BenchmarkMeta 0) [1..] bs)

runMain :: S.Set BenchmarkMeta -> IO ()
runMain = fix (go>=>) . (,) 0
  where
    go (r,s) = case S.minView s of
      Just (BenchmarkMeta{..}, s') ->
            let mv = (length s - position) * 3 + 3
            in do
          B.hPutBuilder stdout
            $ csi' [mv] 'F' -- cursorUpLine mv
            <> indicatorBuilder
            <> csi' [mv] 'E' -- cursorDownLine mv
          ana <- analysis <$> step benchmark
          let newMax | r == mean (analysis benchmark) = mean ana
                     | otherwise = max r $ mean ana
          w <- maybe 60 width <$> size
          result <- evaluate $ renderAnalysis ana
          B.hPutBuilder stdout
            $ csi' [mv] 'F' -- cursorUpLine mv
            <> analysisBuilder result
            <> case () of
                 _ | samples ana <= 1 -> B.char7 '\n'
                   | otherwise  -> barBuilder w (mean ana / newMax) (min 1 $ sigmaLevel * stdError ana / fromRational (mean ana)) (min 1 $ sigma ana / fromRational (mean ana))
            <> csi' [mv-2] 'E' -- cursorDownLine $ mv-2
          pure (newMax, S.insert (BenchmarkMeta (informationOf ana) position benchmark{analysis = ana}) s')
      Nothing -> pure (r,s)

step :: Benchmark -> IO Benchmark
step (Benchmark n f a) = Benchmark n f <$> f a

benchIO :: String -> IO a -> Benchmark
benchIO n io = benchIO' n (Right io)

benchIO' :: String -> Either (Int -> IO a) (IO a) -> Benchmark
benchIO' n io = Benchmark n f (Analysis 0 0 0 0 0)

  where
    f ana = let w = fromIntegral (weightOf ana) in refineAnalysis ana
      <$> getSystemTime
       <* either (void . ($w)) (replicateM_ w) io
      <*> getSystemTime

benchShell :: String -> String -> Benchmark
benchShell n cmd = benchIO' n $ Left $ \times -> withCreateProcess (shell (intercalate ";" $ replicate times cmd)) {std_out = CreatePipe, std_err = CreatePipe} $ \_ _ _ ph ->
  void $ waitForProcess ph

bench :: NFData b => String -> (a -> b) -> a -> Benchmark
bench n f = benchIO n . evaluate . force . f

renderAnalysis :: Analysis -> B.Builder
renderAnalysis a@Analysis{..}
  = B.char7 't' <> B.char7 '='
  <> prettyScientific (fromRational mean) (Just $ sigmaLevel * stdError a)
  <> B.char7 's' <> B.char7 ' '
  <> B.charUtf8 'σ' <> B.char7 '='
  <> prettyScientific (100 * sigma a / fromRational mean) Nothing
  <> B.char7 '%' <> B.char7 ' '
  <> B.char7 'n' <> B.char7 '='
  <> prettyWord samples

sigmaLevel :: Double
sigmaLevel = 6

prettyWord :: Word -> B.Builder
prettyWord = go
  where
    go x = case divMod x 1000 of
             (a,b) | a == 0 -> B.wordDec b
                   | b > 99 -> go a <> B.char7 ',' <> B.wordDec b
                   | b >  9 -> go a <> B.char7 ',' <> B.char7 '0' <> B.wordDec b
                   | otherwise -> go a <> B.char7 ',' <> B.char7 '0' <> B.char7 '0' <> B.wordDec b

prettyScientific :: Double -> Maybe Double -> B.Builder
prettyScientific x b = case floatToDigits 10 <$> b of
    Just (errSig,errExpo) | errSig /= [0] && valLen errExpo > 0 -> mantissa (take (valLen errExpo) $ sig ++ repeat 0) <> showError errSig <> f expo
    _ | x == 0 -> B.char7 '0'
    _ -> mantissa (take 2 $ sig ++ repeat 0) <> f expo
  where

    showError err = B.char7 '(' <> foldMap B.intDec (take 2 $ err ++ repeat 0) <> B.char7 ')'
    (sig,expo) = floatToDigits 10 x
    valLen e = expo - e + 2
    mantissa [d] = B.intDec d
    mantissa (d:ds) = B.intDec d <> B.char7 '.' <> foldMap B.intDec ds
    mantissa [] = mempty
    f 1 = mempty
    f 2 = B.charUtf8 '·' <> B.intDec 10
    f e = B.charUtf8 '·' <> B.intDec 10 <> showE (e-1)

showE :: Integral a => a -> B.Builder
showE = fix go
  where go f = \case
          0 -> B.charUtf8 '⁰'
          1 -> B.charUtf8 '¹'
          2 -> B.charUtf8 '²'
          3 -> B.charUtf8 '³'
          4 -> B.charUtf8 '⁴'
          5 -> B.charUtf8 '⁵'
          6 -> B.charUtf8 '⁶'
          7 -> B.charUtf8 '⁷'
          8 -> B.charUtf8 '⁸'
          9 -> B.charUtf8 '⁹'
          n | n < 0 -> B.charUtf8 '⁻' <> f (abs n)
            | otherwise -> case divMod n 10 of
                (0, b) -> f b
                (a, b) -> f a <> f b

sigma :: Analysis -> Double
sigma = sqrt . fromRational . variance

stdError :: Analysis -> Double
stdError Analysis{..}
   = sqrt (fromRational $ variance * fromIntegral squaredWeights / fromIntegral (samples*samples))

informationOf :: Analysis -> Double
informationOf Analysis{..}
  = (m*m * n*n)
  / (m*m * n + v*w2)
  where
    n = fromIntegral samples
    m = fromRational mean
    v = fromRational variance
    w2 = fromIntegral squaredWeights

weightOf :: Analysis -> Word
weightOf Analysis{..} = max 1 . min samples . round . recip $ sqrt (fromRational mean :: Double)

refineAnalysis :: Analysis -> SystemTime -> SystemTime -> Analysis
refineAnalysis ana@Analysis{..} begin end = Analysis newSamples newSquaredWeights newMean newQFactor newVariance

  where
    newSamples = samples + weightOf ana
    newSquaredWeights = squaredWeights + weightOf ana*weightOf ana
    newMean = mean       + diffWeight / fromIntegral newSamples
    newQFactor = qFactor + diffWeight * (time - newMean)
    newVariance = newQFactor / fromIntegral newSamples

    diffWeight = fromIntegral (weightOf ana) * (time - mean)

    time = (toSeconds end - toSeconds begin) / fromIntegral (weightOf ana)
    toSeconds t = fromIntegral (systemSeconds t) + fromIntegral (systemNanoseconds t) / 1e9

sgrBuilder :: SGR -> B.Builder
sgrBuilder = flip csi' 'm' . sgrToCode

csi' :: [Int] -> Char -> B.Builder
csi' (x:xs) b = B.char7 '\ESC' <> B.char7 '[' <> B.intDec x <> foldMap (\n -> B.char7 ';' <> B.intDec n) xs <> B.char7 b
csi' [] b = B.char7 '\ESC' <> B.char7 '[' <> B.char7 b

indicatorBuilder :: B.Builder
indicatorBuilder = sgrBuilder (SetColor Foreground Vivid Red) <> B.charUtf8 '►' <> sgrBuilder (Reset)

analysisBuilder :: B.Builder -> B.Builder
analysisBuilder result = do
    csi' [2] 'K' -- clearLine
    <> B.char7 ' '
    <> B.char7 ' '
    <> result
    <> B.char7 '\n'

barBuilder :: Int -> Rational -> Double -> Double -> B.Builder
barBuilder width m stdErr sd =
  csi' [2] 'K' -- clearLine
  <> B.char7 ' ' <> B.char7 ' ' <> B.stringUtf8 (replicate (pred valueLength) '▀')
  <> sgrBuilder (SetColor Foreground Dull Magenta)
  <> B.stringUtf8 (replicate errorLength '▀')
  <> middle
  <> sgrBuilder (SetColor Foreground Dull Magenta)
  <> B.stringUtf8 (replicate errorLength '▀')
  <> sgrBuilder (SetColor Foreground Vivid Black)
  <> B.stringUtf8 (replicate sigmaLength '▔')
  <> sgrBuilder Reset
  <> B.char7 '\n'

  where
    middle
      | len * stdErr >= 0.25 = sgrBuilder (SetColor Foreground Vivid Magenta) <> B.charUtf8 '▀'
      | otherwise = mempty
    len = fromRational m * fromIntegral width / 2 - 4
    valueLength = round len - errorLength
    errorLength = round $ len * stdErr
    sigmaLength = round (len * sd) - errorLength

nameBuilder :: String -> B.Builder
nameBuilder n =
  sgrBuilder (SetColor Foreground Vivid Cyan)
  <> fromString n
  <> sgrBuilder Reset
  <> "\n\n\n"
