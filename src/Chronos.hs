{-# LANGUAGE RecordWildCards #-}
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

import qualified Data.ByteString.Builder as B
import qualified Data.Set as S

data Benchmark
  = Benchmark
  { name :: String
  , analysis :: Analysis
  , runner :: Analysis -> IO Analysis
  }

data BenchmarkMeta
  = BenchmarkMeta
  { information :: Double
  , maxDuration :: Rational
  , position :: Int
  , benchmark :: Benchmark
  }

instance Eq BenchmarkMeta where
  (==) = (==) `on` position &&& analysis . benchmark

instance Ord BenchmarkMeta where
  compare = compare `on` information &&& negate . position &&& analysis . benchmark

data Analysis
  = Analysis
  { samples :: Natural
  , squaredWeights :: Natural
  , mean :: Rational
  , qFactor :: Rational
  , variance :: Rational
  } deriving (Eq, Ord, Show, Read)

data Computation where
   Shell :: String -> Computation
   Pure :: NFData b => (a -> b) -> a -> Computation
   Impure :: IO a -> Computation

printBenchmark :: BenchmarkMeta -> IO ()
printBenchmark b = do
  w <- maybe 60 width <$> size
  B.hPutBuilder stdout $ csi' [3*position b-1] 'F' <> renderBenchmark w (maxDuration b) (benchmark b) <> csi' [3*position b-2] 'E'

renderBenchmark :: Int -> Rational -> Benchmark -> B.Builder
renderBenchmark w maxDuration Benchmark{..}
  = B.char7 ' '
  <> B.char7 ' '
  <> renderAnalysis analysis
  <> csi' [0] 'K'
  <> B.char7 '\n'
  <> if samples analysis <= 1
  then mempty
  else barBuilder w (mean analysis / maxDuration) (min 1 $ sigmaLevel * stdError analysis / fromRational (mean analysis)) (min 1 $ sigma analysis / fromRational (mean analysis))

defaultMain :: [Benchmark] -> IO ()
defaultMain bs = bracket_ hideCursor showCursor $
  B.hPutBuilder stdout (foldMap (nameBuilder . name) bs)
  *> foldr1 (>=>) (replicate 10 step) (benchIO "warmup" (return ()))
  *> runMain (S.fromList $ zipWith (BenchmarkMeta 0 0) [1..] $ reverse bs)

runMain :: S.Set BenchmarkMeta -> IO ()
runMain = fix (go>=>) . (,) 0
  where
    go (md, s) = case S.minView s of
      Just (BenchmarkMeta{..}, s') -> do
          ana <- analysis <$> step benchmark
          let newMax | md == mean (analysis benchmark) = mean ana
                     | otherwise = max md $ mean ana
              new = BenchmarkMeta (informationOf ana) newMax position benchmark{analysis = ana}
          printBenchmark new *> pure (newMax, S.insert new s')

      Nothing -> pure (md, s)

{-# INLINE step #-}
step :: Benchmark -> IO Benchmark
step (Benchmark n a f) = flip (Benchmark n) f <$> f a

{-# INLINE benchIO #-}
benchIO :: String -> IO a -> Benchmark
benchIO label io = runComputation (Impure io) label

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
  Shell cmd -> measure go
    where go n = uncurry (>>) $ (flip replicateM_ (f 10000) *** f) (n `divMod` 10000)
          f x = withCreateProcess (shell (intercalate ";" $ replicate x cmd)) {std_out = CreatePipe, std_err = CreatePipe} $ \_ _ _ -> void . waitForProcess

{-# INLINE benchShell #-}
benchShell :: String -> String -> Benchmark
benchShell label cmd = runComputation (Shell cmd) label

{-# INLINE bench #-}
bench :: NFData b => String -> (a -> b) -> a -> Benchmark
bench label f x = runComputation (Pure f x) label

{-# INLINE renderAnalysis #-}
renderAnalysis :: Analysis -> B.Builder
renderAnalysis a@Analysis{..}
  | samples == 0 = mempty
  | otherwise
  = B.char7 't' <> B.char7 '='
  <> prettyScientific (fromRational mean) (Just $ sigmaLevel * stdError a)
  <> B.char7 's' <> B.char7 ' '
  <> B.charUtf8 'σ' <> B.char7 '='
  <> prettyScientific (100 * sigma a / fromRational mean) Nothing
  <> B.char7 '%' <> B.char7 ' '
  <> B.char7 'n' <> B.char7 '='
  <> prettyNatural samples

compareBench :: Double -> Benchmark -> Benchmark -> IO Ordering
compareBench d b1 b2 | ((||) `on` (<3) . samples . analysis) b1 b2
                       || ((||) `on` (<1) . informationOf . analysis) b1 b2 = next
                     | otherwise = case compareMeans (analysis b1) (analysis b2) of
                         EQ | ((||) `on` (not . relativeErrorBelow (d/2) . analysis)) b1 b2 -> next
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
  where go f n | n < 0 = B.charUtf8 '⁻' <> f (abs n)
               | n < 10 = B.charUtf8 $ "⁰¹²³⁴⁵⁶⁷⁸⁹" !! fromIntegral n
               | otherwise = uncurry ((<>) `on` f) $ divMod n 10

sigma :: Analysis -> Double
sigma Analysis{..} = sqrt (fromRational variance) / biasCorrection
  where biasCorrection
          = 1
          - 1/(4*fromIntegral samples)
          - 7/(32*fromIntegral samples**2)
          - 19/(128*fromIntegral samples**3)

stdError :: Analysis -> Double
stdError a@Analysis{..} = sigma a * sqrt (fromIntegral squaredWeights) / fromIntegral samples

informationOf :: Analysis -> Double
informationOf Analysis{..} = sqrt (fromRational mean) * fromIntegral samples

weightOf :: Analysis -> Int
weightOf Analysis{..} = fromIntegral . max 1 . min samples . round . recip $ (fromRational mean :: Double) ** 0.7

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

barBuilder :: Int -> Rational -> Double -> Double -> B.Builder
barBuilder width m stdErr sd =
  B.char7 ' ' <> B.char7 ' ' <> B.stringUtf8 (replicate (pred valueLength) '▀')
  <> sgrBuilder (SetColor Foreground Dull Magenta)
  <> B.stringUtf8 (replicate errorLength '▀')
  <> middle
  <> sgrBuilder (SetColor Foreground Dull Magenta)
  <> B.stringUtf8 (replicate errorLength '▀')
  <> sgrBuilder (SetColor Foreground Vivid Black)
  <> B.stringUtf8 (replicate sigmaLength '▔')
  <> sgrBuilder Reset
  <> csi' [0] 'K' -- clearLine

  where
    middle
      | len * stdErr >= 0.20 = sgrBuilder (SetColor Foreground Vivid Magenta) <> B.charUtf8 '▀'
      | otherwise = mempty
    len = fromRational m * fromIntegral (width - 6) / 2
    valueLength = round len - errorLength
    errorLength = round $ len * stdErr
    sigmaLength = round (len * sd) - errorLength

nameBuilder :: String -> B.Builder
nameBuilder n =
  sgrBuilder (SetColor Foreground Vivid Cyan)
  <> fromString n
  <> sgrBuilder Reset
  <> fromString "\n\n\n"
