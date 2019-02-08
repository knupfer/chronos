{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Chronos where

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

type Name = String

data Benchmark
  = Benchmark
  { name :: Name
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

variance :: Analysis -> Rational
variance Analysis{..}
  | samples <= 1 = 0
  | otherwise = qFactor / fromIntegral (samples - 1)

sigma :: Analysis -> Double
sigma = sqrt . fromRational . variance

stdError :: Analysis -> Double
stdError Analysis{..}
  | samples > 1 = sqrt $ fromRational (fromIntegral squaredWeights * qFactor / fromIntegral (samples - 1)) / fromIntegral samples
  | otherwise = 0

step :: Benchmark -> IO Benchmark
step (Benchmark n f a) = Benchmark n f <$> f a

benchIO :: Name -> Either (Int -> IO a) (IO a) -> Benchmark
benchIO n io = Benchmark n f (Analysis 0 0 0 0)

  where
    f Analysis{..} = do
      let weight | mean == 0 = 1
                 | otherwise = max 1 (min samples . round $ 1 / sqrt (fromRational mean :: Double))
      time <- runBench (fromIntegral weight)
      let newSamples = samples + weight
          newMean = mean + fromIntegral weight * (time - mean) / fromIntegral newSamples
          newQFactor = qFactor + fromIntegral weight * (time - mean) * (time - newMean)
          newSquaredWeights = squaredWeights + weight*weight

      return $ Analysis newSamples newSquaredWeights newMean newQFactor

    runBench weight = do
            begin <- getSystemTime
            either (\i -> void $ i weight) (replicateM_ weight) io
            end <- getSystemTime
            return $ (toSeconds end - toSeconds begin) / fromIntegral weight
    toSeconds t = fromIntegral (systemSeconds t) + fromIntegral (systemNanoseconds t) / 1000000000

benchShell :: Name -> String -> Benchmark
benchShell n cmd = benchIO n $ Left $ \times -> withCreateProcess (shell (intercalate ";" $ replicate times cmd)) {std_out = CreatePipe, std_err = CreatePipe} $ \_ _ _ ph ->
  void $ waitForProcess ph

bench :: NFData b => Name -> (a -> b) -> a -> Benchmark
bench n f = benchIO n . Right . evaluate . force . f

defaultMain :: [Benchmark] -> IO ()
defaultMain bs = bracket_ hideCursor showCursor $ do
  forM_ bs $ \b -> do
    printName (name b)
    replicateM_ 2 (putStrLn "")

  putStrLn ""
  runMain . S.fromList $ zipWith (BenchmarkMeta 0) [1..] bs

runMain :: S.Set BenchmarkMeta -> IO ()
runMain = fix (go>=>) . (,) 0
  where
    go (r,s) = case S.minView s of
      Nothing -> pure (r,s)
      Just (BenchmarkMeta{..}, s') ->
            let mv = (length s - position) * 3 + 1
            in bracket_ (cursorUpLine $ mv+2) (cursorDownLine mv) $ do
          printIndicator
          cursorDownLine $ mv+2
          ana <- analysis <$> step benchmark
          cursorUpLine $ mv+2
          printAnalysis ana
          let r' | r == mean (analysis benchmark) = mean ana
                 | otherwise = max r $ mean ana
              info = recip $ 1/ 1.5^samples ana + (stdError ana / fromRational (mean ana)) / fromIntegral (samples ana)
          printBar (mean ana / r') $ sigma ana / fromRational (mean ana)
          pure (r', S.insert (BenchmarkMeta info position benchmark{analysis = ana}) s')

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
    [ "x=", prettyScientific (fromRational mean) (Just $ stdError a), "s "
    , "σ=", prettyScientific (100*sigma a/ fromRational mean) Nothing, "% "
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
