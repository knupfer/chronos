{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Chronos where

import Control.Arrow
import Numeric
import Numeric.Natural
import Data.List
import System.IO.Unsafe
import Data.Time.Clock.System
import Control.Monad
import System.Console.ANSI
import Control.Exception
import Control.DeepSeq
import System.Process

type Name = String

data Param = Relative | Absolute

data Benchmark a = Benchmark Name [Analysis a]

benchIO :: Name -> IO a -> IO (Benchmark 'Absolute)
benchIO n io = fmap (Benchmark n) (analyse <$> go)
  where
    go = unsafeInterleaveIO $ do
            begin <- getSystemTime
            void $ io
            end <- getSystemTime
            let x = toSeconds end - toSeconds begin
            (x:) <$> go
    toSeconds t = fromIntegral (systemSeconds t) + fromIntegral (systemNanoseconds t) / 1000000000

benchShell :: Name -> String -> IO (Benchmark 'Absolute)
benchShell n cmd = benchIO n $ withCreateProcess (shell cmd) {std_out = CreatePipe, std_err = CreatePipe} $ \_ _ _ ph ->
  void $ waitForProcess ph

bench :: NFData b => Name -> (a -> b) -> a -> IO (Benchmark 'Absolute)
bench n f = benchIO n . evaluate . force . f

defaultMain :: [IO (Benchmark 'Absolute)] -> IO ()
defaultMain ioBench = bracket_ hideCursor showCursor $ do
  xs <- sequenceA ioBench
  let benchs = map (\(Benchmark n as) -> (n, map relative as)) xs
  mapM_ (\b -> printName (fst b) >> putStrLn "") benchs
  printer . zip [1..] $ map snd benchs

printName :: Name -> IO ()
printName name = do
  clearLine
  setSGR [SetColor Foreground Vivid Cyan]
  putStrLn name
  clearLine
  setSGR [Reset]

printer :: [(Int, [Analysis 'Relative])] -> IO ()
printer xs = do
    mapM_ update xs
    go xs
  where
    go lst@(z:_) = update z >> go (increasePrecision lst)
    go [] = error "impossible!"
    update :: (Int, [Analysis 'Relative]) -> IO ()
    update (n,bs) = do
      let mv = ((length xs - n) + 1) * 2
      bracket_ (cursorUpLine mv)
               (cursorDownLine mv) $ do
          cursorDownLine 1
          setSGR [SetColor Foreground Vivid Red]
          putStrLn ">"
          setSGR [Reset]
          cursorUpLine 2
          f bs
          cursorUpLine 2

    f (a:_) = do
      cursorDownLine 1
      clearLine
      putStrLn ("  " ++ show a)
    f _ = error "impossible!"
    increasePrecision
      = (\(a:bs) -> second tail a:bs)
      . reverse
      . sortOn ((\ana -> 1/ 2^samples ana + stdError ana / sqrt (fromIntegral (samples ana))) . head . snd)

data Analysis (x :: Param)
  = Analysis
  { samples
    :: Natural
  , variance
    :: Rational
  , mean
    :: Rational
  }

instance Show (Analysis 'Absolute) where
  show a@Analysis{..}
    = unwords
    [ "x=" ++ prettyScientific (fromRational mean :: Double) meanLen "s"
    , "SE=" ++ prettyScientific (stdError a) 2 "s"
    , "SD=" ++ prettyScientific (sigma a) 2 "s"
    , "VAR=" ++ prettyScientific (fromRational variance :: Double) 2 "s²"
    , "n=" ++ show samples
    ] where meanLen = snd (floatToDigits 10 (fromRational mean :: Double)) - snd (floatToDigits 10 (stdError a))

instance Show (Analysis 'Relative) where
  show a@Analysis{..}
    = unwords
    [ "x=" ++ prettyScientific (fromRational mean :: Double) meanLen "s"
    , "SE=" ++ prettyScientific (stdError a*100) 2 "%"
    , "SD=" ++ prettyScientific (sigma a*100) 2 "%"
    , "VAR=" ++ prettyScientific (fromRational $ variance*100 :: Double) 2 "%"
    , "n=" ++ show samples
    ] where meanLen = snd (floatToDigits 10 (fromRational mean :: Double)) - snd (floatToDigits 10 (stdError $ absolute a))

prettyScientific :: RealFloat a => a -> Int -> String -> String
prettyScientific x n unit | n < 1 = prettyScientific x 1 unit
prettyScientific x n unit = (\(ds, e) -> g (significants $ ds ++ repeat 0) ++ f e ++ unit) (floatToDigits 10 x)
  where
    significants xs = take n $ case uncons (reverse (take (n+1) xs)) of
      Just (b, bs) | b >= 5 -> reverse $ carryOver bs
      _ -> xs
    carryOver xs = case uncons xs of
      Nothing -> [1]
      Just (9, bs) -> 0 : carryOver bs
      Just (r, bs) -> (r+1) : bs

    g (d:ds) | n == 1 = show d
             | otherwise = show d ++ '.' : concatMap show ds
    g [] = ""
    f 1 = ""
    f 2 = "·" ++ "10"
    f e = "·" ++ "10" ++ showE (e-1)
    showE 0 = "⁰"
    showE 1 = "¹"
    showE 2 = "²"
    showE 3 = "³"
    showE 4 = "⁴"
    showE 5 = "⁵"
    showE 6 = "⁶"
    showE 7 = "⁷"
    showE 8 = "⁸"
    showE 9 = "⁹"
    showE e | e < 0     = '⁻' : showE (negate e)
            | otherwise = (\(a, b) -> showE a ++ showE b) $ divMod e 10

analyse :: [Rational] -> [Analysis 'Absolute]
analyse [] = []
analyse (y:ys)
  = Analysis 1 0 y:zipWith3 (\m q n -> Analysis n (q / fromIntegral (n-1)) m) (tail means) (tail qs) [2..]

  where

    means = y:zipWith3 (\m x n -> m + (x - m) / n) means ys [2..]
    qs = 0:zipWith3 (\q x (m, m') -> q + (x - m) * (x - m')) qs ys (zip means (tail means))

sigma :: Analysis a -> Double
sigma = sqrt . fromRational . variance

stdError :: Analysis a -> Double
stdError = uncurry (/) . (sigma &&& sqrt . fromIntegral . samples)

relative :: Analysis 'Absolute -> Analysis 'Relative
relative x = x{variance = variance x / mean x^(2::Int)}

absolute :: Analysis 'Relative -> Analysis 'Absolute
absolute x = x{variance = variance x * mean x^(2::Int)}
