{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  , variance
  , sigma
  , stdError
  , step
  ) where

import Control.Arrow
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

import qualified Data.Set as S

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
  { samples :: Natural
  , squaredWeights :: Natural
  , mean :: Rational
  , qFactor :: Rational
  } deriving (Eq, Ord)

instance Show Analysis where
  show a@Analysis{..}
    = concat
    [ "x=", prettyScientific (fromRational mean) (Just $ stdError a), "s "
    , "σ=", prettyScientific (100 * sigma a / fromRational mean) Nothing, "% "
    , "n=", showNumWithSpaces samples
    ]

showNumWithSpaces :: (Show a) => a -> String
showNumWithSpaces x = reverse . intercalate "," . fix go $ reverse (show x)
  where go f ds | length ds > 3 = let (a,b) = splitAt 3 ds in a : f b
                | otherwise = [ds]

ord0 :: Int
ord0 = ord '0'

ordDot :: Int
ordDot = ord '.'

prettyScientific :: Double -> Maybe Double -> String
prettyScientific x b = case floatToDigits 10 <$> b of
    Just (errSig,errExpo) | errSig /= [0] -> mantissa (take (max 1 $ valLen errExpo) $ sig ++ repeat 0) ++ showError errSig ++ f expo
    _ | x == 0 -> "0"
    _ -> mantissa (take 2 $ sig ++ repeat 0) ++ f expo
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
digitList = fix go
   where go f x = case divMod x 10 of
          (0, b) -> [b]
          (a, b) -> f a ++ [b]

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

variance :: Analysis -> Rational
variance Analysis{..}
  | samples > 1 = qFactor / fromIntegral (samples - 1)
  | otherwise = 0

sigma :: Analysis -> Double
sigma = sqrt . fromRational . variance

stdError :: Analysis -> Double
stdError ana@Analysis{..}
  | samples > 1 = sqrt (fromIntegral squaredWeights / fromIntegral (samples*samples)) * sigma ana
  | otherwise = 0

weightOf :: Num a => Analysis -> a
weightOf Analysis{..} | mean /= 0 = fromIntegral . max 1 . min samples . round $ 1 / sqrt (fromRational mean :: Double)
                      | otherwise = 1

informationOf :: Analysis -> Double
informationOf ana@Analysis{..} = recip $ recip (1.5^samples) + stdError ana / fromRational mean / fromIntegral samples

refineAnalysis :: Analysis -> SystemTime -> SystemTime -> Analysis
refineAnalysis ana@Analysis{..} begin end = Analysis newSamples newSquaredWeights newMean newQFactor

  where
    newSamples = samples + weight
    newMean = mean + weight * (time - mean) / fromIntegral newSamples
    newQFactor = qFactor + weight * (time - mean) * (time - newMean)
    newSquaredWeights = squaredWeights + weight*weight

    weight :: Num a => a
    weight = weightOf ana

    time = (toSeconds end - toSeconds begin) / weight
    toSeconds t = fromIntegral (systemSeconds t) + fromIntegral (systemNanoseconds t) / 1000000000

benchIO :: String -> IO a -> Benchmark
benchIO n io = benchIO' n (Right io)

benchIO' :: String -> Either (Int -> IO a) (IO a) -> Benchmark
benchIO' n io = Benchmark n f (Analysis 0 0 0 0)

  where
    f ana = let w = weightOf ana in refineAnalysis ana
      <$> getSystemTime
       <* either (void . ($w)) (replicateM_ w) io
      <*> getSystemTime

benchShell :: String -> String -> Benchmark
benchShell n cmd = benchIO' n $ Left $ \times -> withCreateProcess (shell (intercalate ";" $ replicate times cmd)) {std_out = CreatePipe, std_err = CreatePipe} $ \_ _ _ ph ->
  void $ waitForProcess ph

bench :: NFData b => String -> (a -> b) -> a -> Benchmark
bench n f = benchIO n . evaluate . force . f

step :: Benchmark -> IO Benchmark
step (Benchmark n f a) = Benchmark n f <$> f a

defaultMain :: [Benchmark] -> IO ()
defaultMain bs = bracket_ hideCursor showCursor $
  mapM_ (\b -> printName (name b) *> replicateM_ 2 (putStrLn "")) bs
  *> putStrLn ""
  *> runMain (S.fromList $ zipWith (BenchmarkMeta 0) [1..] bs)

runMain :: S.Set BenchmarkMeta -> IO ()
runMain = fix (go>=>) . (,) 0
  where
    go (r,s) = case S.minView s of
      Nothing -> pure (r,s)
      Just (BenchmarkMeta{..}, s') ->
            let mv = (length s - position) * 3 + 3
            in bracket_ (cursorUpLine mv) (cursorDownLine $ mv-2) $ do
          printIndicator
          cursorDownLine mv
          ana <- analysis <$> step benchmark
          cursorUpLine mv
          printAnalysis ana
          let newMax | r == mean (analysis benchmark) = mean ana
                     | otherwise = max r $ mean ana
          printBar (mean ana / newMax) $ sigma ana / fromRational (mean ana)
          pure (newMax, S.insert (BenchmarkMeta (informationOf ana) position benchmark{analysis = ana}) s')

printIndicator :: IO ()
printIndicator = do
  setSGR [SetColor Foreground Vivid Red]
  putChar '►'
  setSGR [Reset]
  hFlush stdout

printAnalysis :: Analysis -> IO ()
printAnalysis ana = do
  result <- evaluate . force $ show ana
  clearLine
  putStrLn (' ':' ':result)

printBar :: Rational -> Double -> IO ()
printBar m sd = do
  clearLine
  putStrLn $ ' ':' ':replicate (round len) '█' ++ take 40 (replicate (round $ fromRational len * sd) '─')
  where len = m * 60

printName :: String -> IO ()
printName n = do
  clearLine
  setSGR [SetColor Foreground Vivid Cyan]
  putStrLn n
  clearLine
  setSGR [Reset]
