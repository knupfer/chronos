{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Chronos where

import Control.Arrow
import Numeric
import Numeric.Natural
import Data.List
import System.IO
import System.IO.Unsafe
import Data.Time.Clock.System
import Control.Monad
import System.Console.ANSI
import Control.Exception
import Control.DeepSeq
import System.Process

type Name = String

data Benchmark = Benchmark Name [Analysis]

benchIO :: Name -> IO a -> IO (Benchmark)
benchIO n io = fmap (Benchmark n) (analyse <$> go)
  where
    go = unsafeInterleaveIO $ do
            overhead <- getSystemTime
            begin <- getSystemTime
            void $ io
            end <- getSystemTime

            let x = toSeconds end + toSeconds overhead - 2*toSeconds begin
            (x:) <$> go
    toSeconds t = fromIntegral (systemSeconds t) + fromIntegral (systemNanoseconds t) / 1000000000

benchShell :: Name -> String -> IO (Benchmark)
benchShell n cmd = benchIO n $ withCreateProcess (shell cmd) {std_out = CreatePipe, std_err = CreatePipe} $ \_ _ _ ph ->
  void $ waitForProcess ph

bench :: NFData b => Name -> (a -> b) -> a -> IO (Benchmark)
bench n f = benchIO n . evaluate . force . f

defaultMain :: [IO Benchmark] -> IO ()
defaultMain ioBench = bracket_ hideCursor showCursor $ do
  xs <- sequenceA ioBench
  let benchs = map (\(Benchmark n as) -> (n, as)) xs
  mapM_ (\b -> printName (fst b) >> putStrLn "" >> putStrLn "") benchs
  printer . zip [1..] $ map snd benchs

printBar :: Analysis -> IO ()
printBar a = putStrLn (take 100 $ "  " ++ replicate (round (mean a * 60)) '█' ++ replicate (round (fromInteger (round (mean a * 60)) * sigma a)) '─')

printName :: Name -> IO ()
printName name = do
  clearLine
  setSGR [SetColor Foreground Vivid Cyan]
  putStrLn name
  clearLine
  setSGR [Reset]

printer :: [(Int, [Analysis])] -> IO ()
printer xs = do
    mapM_ (update False . pure) xs
    go xs
  where
    go zs = do
      update True zs
      go (increasePrecision zs)
    update :: Bool -> [(Int, [Analysis])] -> IO ()
    update _ [] = return ()
    update b ((n,bs):zs) = do
      let mv = ((length xs - n) + 1) * 3
      bracket_ (cursorUpLine mv) (cursorDownLine mv) $ do
        cursorDownLine 1
        setSGR [SetColor Foreground Vivid Red]
        putStrLn "►"
        setSGR [Reset]
        cursorUpLine 2
        cursorDownLine mv
        hFlush stdout
        new <- evaluate $ force $ show $ head bs
        cursorUpLine mv
        cursorDownLine 1
        clearLine
        putStrLn ("  " ++ new)
        if b
          then clearLine >> printBar ((relative $ head bs) {mean = mean (head bs) / (maximum $  map (mean . head . snd) ((n,bs):zs))})
          else putStrLn ""
        cursorUpLine 3

    increasePrecision
      = (\(a:bs) -> fastDrop a:bs)
      . reverse
      . sortOn ((\ana -> 1/ 2^samples ana + stdError (relative ana) / sqrt (fromIntegral (samples ana))) . head . snd)
        where fastDrop a = second (drop (max 1 (min (10 - fromIntegral (samples (head (snd a))) `mod` 10) $ round ((1/100) / (max (1/10000) $ mean (head (snd a))))))) a

data Analysis
  = Analysis
  { samples
    :: Natural
  , variance
    :: Rational
  , mean
    :: Rational
  }

instance Show Analysis where
  show a@Analysis{..}
    = unwords
    [ "x=" ++ prettyScientific (fromRational mean :: Double) (Just $ stdError a) 2 "s"
    , "σ=" ++ prettyScientific ((100*) . sigma $ relative  a) Nothing 2 "%"
    , "n=" ++ show samples
    ]

prettyScientific :: (RealFloat a) => a -> Maybe Double -> Int -> String -> String
prettyScientific x b n unit | n < 1 = prettyScientific x b 1 unit
prettyScientific x b n unit = (\(ds, e) -> mantissa (take (maybe n valLen b) $ ds ++ repeat 0) ++ maybe "" showError b ++ f e ++ unit) (floatToDigits 10 x)
  where

    showError err = "(" ++ concatMap show (take n $ fst (floatToDigits 10 err) ++ repeat 0) ++ ")"

    valLen e = snd (floatToDigits 10 x) - snd (floatToDigits 10 e) + n
    mantissa (d:ds) | n == 1 = show d
             | otherwise = show d ++ '.' : concatMap show ds
    mantissa [] = ""
    f 1 = ""
    f 2 = "·" ++ "10"
    f e = "·" ++ "10" ++ showE (e-1)

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

analyse :: [Rational] -> [Analysis]
analyse [] = []
analyse (y:ys)
  = Analysis 1 0 y:zipWith3 (\m q n -> Analysis n (q / fromIntegral (n-1)) m) (tail means) (tail qs) [2..]

  where

    means = y:zipWith3 (\m x n -> m + (x - m) / n) means ys [2..]
    qs = 0:zipWith3 (\q x (m, m') -> q + (x - m) * (x - m')) qs ys (zip means (tail means))

sigma :: Analysis -> Double
sigma = sqrt . fromRational . variance

stdError :: Analysis -> Double
stdError = uncurry (/) . (sigma &&& sqrt . fromIntegral . samples)

relative :: Analysis -> Analysis
relative x = x{variance = variance x / mean x^(2::Int)}

absolute :: Analysis -> Analysis
absolute x = x{variance = variance x * mean x^(2::Int)}
