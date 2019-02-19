{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

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
  , isEqualTo
  , isFasterThan
  , compareBench
  ) where

import Data.IORef
import Data.String
import Control.Arrow
import Data.Function
import Numeric
import Numeric.Natural
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
import System.Mem

import qualified Data.Set as S
import qualified Data.ByteString.Builder as B

data Benchmark
  = Benchmark
  { name :: String
  , analysis :: Analysis
  , runner :: Analysis -> IO Analysis
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
  { samples :: Natural
  , squaredWeights :: Natural
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
          B.hPutBuilder stdout
            $ csi' [mv] 'F' -- cursorUpLine mv
            <> analysisBuilder (renderAnalysis ana)
            <> case () of
                 _ | samples ana <= 1 -> B.char7 '\n'
                   | otherwise  -> barBuilder w (mean ana / newMax) (min 1 $ sigmaLevel * stdError ana / fromRational (mean ana)) (min 1 $ sigma ana / fromRational (mean ana))
            <> csi' [mv-2] 'E' -- cursorDownLine $ mv-2
          pure (newMax, S.insert (BenchmarkMeta (informationOf ana) position benchmark{analysis = ana}) s')
      Nothing -> pure (r,s)

step :: Benchmark -> IO Benchmark
step (Benchmark n a f) = flip (Benchmark n) f <$> f a

{-# INLINE benchIO #-}
benchIO :: String -> IO a -> Benchmark
benchIO label io = runComputation (Impure io) label

data Computation where
   Shell :: String -> Computation
   Pure :: NFData b => (a -> b) -> a -> Computation
   Impure :: IO a -> Computation

{-# INLINE measure #-}
measure :: (Int -> IO a) -> Analysis -> IO Analysis
measure action ana
  = performMinorGC
  >> refineAnalysis ana
  <$> getSystemTime
  <* action (weightOf ana)
  <*> getSystemTime

{-# INLINE runComputation #-}
runComputation :: Computation -> String -> Benchmark
runComputation comp label = Benchmark label (Analysis 0 0 0 0 0) $ case comp of
  Impure io -> measure (flip replicateM_ io)
  Pure g x  -> \ana -> newIORef x >>= \io -> (measure (\n -> replicateM_ n $ (return$!) . force . g =<< readIORef io) ana)
  Shell cmd -> measure (\n -> withCreateProcess (shell (intercalate ";" $ replicate n cmd)) {std_out = CreatePipe, std_err = CreatePipe} $ \_ _ _ -> void . waitForProcess)

{-# INLINE benchShell #-}
benchShell :: String -> String -> Benchmark
benchShell label cmd = runComputation (Shell cmd) label

{-# INLINE bench #-}
bench :: NFData b => String -> (a -> b) -> a -> Benchmark
bench label f x = runComputation (Pure f x) label

{-# INLINE renderAnalysis #-}
renderAnalysis :: Analysis -> B.Builder
renderAnalysis a@Analysis{..}
  = B.char7 't' <> B.char7 '='
  <> prettyScientific (fromRational mean) (Just $ sigmaLevel * stdError a)
  <> B.char7 's' <> B.char7 ' '
  <> B.charUtf8 'σ' <> B.char7 '='
  <> prettyScientific (100 * sigma a / fromRational mean) Nothing
  <> B.char7 '%' <> B.char7 ' '
  <> B.char7 'n' <> B.char7 '='
  <> prettyNatural samples

compareBench :: Double -> Benchmark -> Benchmark -> IO Ordering
compareBench d b1 b2 | ((||) `on` ((<2) . samples . analysis)) b1 b2
                       || ((||) `on` (\b -> mean (analysis b) * fromIntegral (samples (analysis b)) < 0.1)) b1 b2 = next
                     | otherwise = case compareMeans (analysis b1) (analysis b2) of
                         EQ | ((&&) `on` (relativeErrorBelow (d/2) . analysis)) b1 b2 -> pure EQ
                            | otherwise -> next
                         r -> pure r

   where next | ((<=) `on` informationOf . analysis) b1 b2 = flip (compareBench d) b2 =<< step b1
              | otherwise = compareBench d b1 =<< step b2

relativeErrorBelow :: Double -> Analysis -> Bool
relativeErrorBelow d a = d > sigmaLevel * stdError a / fromRational (mean a)

compareMeans :: Analysis -> Analysis -> Ordering
compareMeans a1 a2
  | f a1 a2 = LT
  | f a2 a1 = GT
  | otherwise = EQ
  where f x y = fromRational (mean x) + sigmaLevel*stdError x < fromRational (mean y) - sigmaLevel*stdError y

isEqualTo :: Benchmark -> Benchmark -> IO Bool
isEqualTo b1 b2 = (EQ==) <$> compareBench 0.01 b1 b2

isFasterThan :: Benchmark -> Benchmark -> IO Bool
isFasterThan b1 b2 = (LT==) <$> compareBench 0.01 b1 b2

sigmaLevel :: Double
sigmaLevel = 6

prettyNatural :: Natural -> B.Builder
prettyNatural = go . fromIntegral
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
stdError a@Analysis{..}
   = sigma a * sqrt (fromIntegral squaredWeights) / fromIntegral samples

informationOf :: Analysis -> Double
informationOf Analysis{..}
  | n == 0 = 0
  | otherwise = (m*m * n*n*n)
  / (m*m * n + v*w2)
  where
    n = fromIntegral samples
    m = fromRational mean
    v = fromRational variance
    w2 = fromIntegral squaredWeights

weightOf :: Analysis -> Int
weightOf Analysis{..} = fromIntegral . max 1 . min samples . round . recip $ sqrt (fromRational mean :: Double)

{-# INLINE refineAnalysis #-}
refineAnalysis :: Analysis -> SystemTime -> SystemTime -> Analysis
refineAnalysis ana@Analysis{..} begin end = Analysis newSamples newSquaredWeights newMean newQFactor newVariance

  where
    newSamples = samples + fromIntegral (weightOf ana)
    newSquaredWeights = squaredWeights + fromIntegral (weightOf ana*weightOf ana)
    newMean = mean + diffWeight / fromIntegral newSamples
    newQFactor = qFactor + diffWeight * (time - newMean)
    newVariance | newSamples > 1 = newQFactor / fromIntegral (newSamples - 1)
                | otherwise = 0

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
      | len * stdErr >= 0.20 = sgrBuilder (SetColor Foreground Vivid Magenta) <> B.charUtf8 '▀'
      | otherwise = mempty
    len = fromRational m * fromIntegral (width - 2) / 2
    valueLength = round len - errorLength
    errorLength = round $ len * stdErr
    sigmaLength = round (len * sd) - errorLength

nameBuilder :: String -> B.Builder
nameBuilder n =
  sgrBuilder (SetColor Foreground Vivid Cyan)
  <> fromString n
  <> sgrBuilder Reset
  <> "\n\n\n"
