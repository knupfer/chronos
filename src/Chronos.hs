{-# LANGUAGE RecordWildCards #-}

module Chronos
  ( Benchmark(..)
  , defaultMain
  , bench
  , benchIO
  , benchShell
  , isEqualTo
  , isFasterThan
  , defaultMainWith
  , defaultConfig
  , sigma
  , stdError
  , step
  , Analysis(..)
  , Config(..)
  ) where

import Parser

import Control.Applicative
import Options.Applicative
import Data.Functor
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
import System.Console.ANSI hiding (clearLine)
import System.Console.ANSI.Codes
import Control.Exception
import Control.DeepSeq
import System.Process
import System.Console.Terminal.Size
import System.Mem

import qualified Data.ByteString.Builder as B
import qualified Data.Set as S

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

data Config
  = Config
  { hideBar :: Bool
  , sameLine :: Bool
  , hideDetails :: Bool
  , printOnce :: Bool
  , sortByMean :: Bool
  , simple :: Bool
  , sigmaLevel :: Double
  , timeout :: Maybe Double
  , maxRelativeError :: Maybe Double
  }

data Benchmark
  = Benchmark
  { name :: String
  , analysis :: Analysis
  , runner :: Analysis -> IO Analysis
  }

data Analysis
  = Analysis
  { samples :: Natural
  , squaredWeights :: Natural
  , mean :: Rational
  , qFactor :: Rational
  } deriving (Eq, Ord, Show, Read)

{-# INLINE step #-}
step :: Benchmark -> IO Benchmark
step (Benchmark n a f) = flip (Benchmark n) f <$> f a

sigma :: Analysis -> Double
sigma a = sqrt (fromRational $ variance a) / biasCorrection
  where biasCorrection
          = 1
          - 1/(4*fromIntegral (samples a))
          - 7/(32*fromIntegral (samples a)**2)
          - 19/(128*fromIntegral (samples a)**3)

stdError :: Analysis -> Double
stdError a | samples a == 1 = fromRational (mean a)
           | otherwise = sigma a * sqrt (fromIntegral $ squaredWeights a) / fromIntegral (samples a)

variance :: Analysis -> Rational
variance a | samples a > 1 = qFactor a / fromIntegral (samples a - 1)
           | otherwise = 0

defaultConfig :: Config
defaultConfig = Config
  { hideBar = False
  , sameLine = False
  , hideDetails = False
  , printOnce = False
  , sortByMean = False
  , simple = False
  , sigmaLevel = 6
  , timeout = Nothing
  , maxRelativeError = Nothing
  }

defaultMain :: [Benchmark] -> IO ()
defaultMain bs = flip defaultMainWith bs =<< execParser opts
  where
    opts = info (configParser Config <**> helper) fullDesc

printBenchmark :: Config -> BenchmarkMeta -> IO ()
printBenchmark cfg b = do
  w <- maybe 60 width <$> size
  B.hPutBuilder stdout . mv $ renderBenchmark cfg w (maxDuration b) (benchmark b)
  where mv x | sortByMean cfg || printOnce cfg = x
             | otherwise = linesUp (printHeight cfg*position b) <> x <> linesDown (printHeight cfg*(position b-1))

linesUp :: Int -> B.Builder
linesUp n | n > 0 = csi' [n] 'F'
          | n < 0 = csi' [abs n] 'E'
          | otherwise = mempty

linesDown :: Int -> B.Builder
linesDown = linesUp . negate

clearLine :: Config -> B.Builder
clearLine cfg | printOnce cfg = mempty
              | otherwise = csi' [0] 'K'

mUnless :: Monoid m => Bool -> m -> m
mUnless t = mWhen (not t)

mWhen :: Monoid m => Bool -> m -> m
mWhen t x = if t then x else mempty

renderBenchmark :: Config -> Int -> Rational -> Benchmark -> B.Builder
renderBenchmark cfg w maxDuration Benchmark{..}
  = mUnless (simple cfg) (sgrBuilder $ SetColor Foreground Vivid Cyan)
  <> fromString name
  <> mUnless (simple cfg) (sgrBuilder Reset)
  <> mUnless (sameLine cfg) (clearLine cfg <> B.char7 '\n' <> B.char7 ' ')
  <> B.char7 ' '
  <> renderAnalysis cfg analysis
  <> clearLine cfg
  <> B.char7 '\n'
  <> mUnless (hideBar cfg)
  ( mUnless (samples analysis <= 1)
    ( barBuilder cfg w (mean analysis / maxDuration) (min 1 $ sigmaLevel cfg * stdError analysis / fromRational (mean analysis)) (min 1 $ sigma analysis / fromRational (mean analysis))
      <> clearLine cfg
    ) <> B.char7 '\n'
  )

defaultMainWith :: Config -> [Benchmark] -> IO ()
defaultMainWith _ [] = pure ()
defaultMainWith cfg bs | printOnce cfg = go (pure ())
                       | otherwise = bracket_ hideCursor showCursor
                         . go . B.hPutBuilder stdout . fromString $ replicate (printHeight cfg*length bs) '\n'

  where go mkSpace = hSetEcho stdin False *> mkSpace *> warmup *> runMain cfg (S.fromList . zipWith (BenchmarkMeta 0 0) [1..] $ reverse pad)
        pad | sameLine cfg = let len = maximum (map (length . name) bs) in map (\x -> x{name = take len $ name x ++ repeat ' '}) bs
            | otherwise = bs

printHeight :: Config -> Int
printHeight cfg = 3 - fromEnum (hideBar cfg) - fromEnum (sameLine cfg)

runMain :: Config -> S.Set BenchmarkMeta -> IO ()
runMain cfg = printAll <=< go . (,) 0
  where
    go (m, s) = handleJust (\e -> if e == UserInterrupt then Just s else Nothing) pure $
        let (BenchmarkMeta{..}, s') = S.deleteFindMin s in do
              ana <- analysis <$> step benchmark
              let newMax | m == mean (analysis benchmark) = mean ana
                         | otherwise = max m $ mean ana
                  new = BenchmarkMeta (informationOf ana) newMax position benchmark{analysis = ana}
                  set = S.insert new s'
              mask_ $ pp new set

              if terminates set
                 then pure set
                 else go (newMax, set)

    f | sortByMean cfg = sortOn (negate . mean . analysis . benchmark)
      | otherwise = sortOn (negate . position)

    printAll set = do
      when (sortByMean cfg && not (printOnce cfg)) . B.hPutBuilder stdout . linesUp $ printHeight cfg*length set
      mapM_ (printBenchmark cfg) . f $ S.toList set

    terminates set = let as = map (analysis . benchmark) $ S.toList set
      in maybe False (<= fromRational (sum $ map (uncurry (*) . (mean &&& fromIntegral . samples)) as)) (timeout cfg)
      || maybe False (>= maximum (map (uncurry (/) . ((sigmaLevel cfg*) . stdError &&& fromRational . mean)) as)) (maxRelativeError cfg)

    pp n set
      | printOnce cfg = pure ()
      | sortByMean cfg = printAll set
      | otherwise = printBenchmark cfg n

{-# INLINE measure #-}
measure :: (Int -> IO a) -> Analysis -> IO Analysis
measure cmd ana
  = performMinorGC
  >> refineAnalysis ana
  <$> getSystemTime
  <* cmd (fromIntegral $ weightOf ana)
  <*> getSystemTime

{-# INLINE benchIO #-}
benchIO :: String -> IO a -> Benchmark
benchIO label io = Benchmark label (Analysis 0 0 0 0) (measure (`replicateM_` io))

{-# INLINE bench #-}
bench :: NFData b => String -> (a -> b) -> a -> Benchmark
bench label f x = Benchmark label (Analysis 0 0 0 0) $ \ana -> newIORef x >>= \io -> measure (\n -> replicateM_ n $ (return$!) . force . f =<< readIORef io) ana

{-# INLINE benchShell #-}
benchShell :: String -> String -> Benchmark
benchShell label cmd = Benchmark label (Analysis 0 0 0 0) $ measure go
    where go n = uncurry (>>) $ ((`replicateM_` f 10000) *** f) (n `divMod` 10000)
          f x = withCreateProcess (shell (intercalate ";" $ replicate x cmd)) {std_out = CreatePipe, std_err = CreatePipe, delegate_ctlc = True} $ \_ _ _ -> void . waitForProcess

{-# INLINE renderAnalysis #-}
renderAnalysis :: Config -> Analysis -> B.Builder
renderAnalysis cfg a@Analysis{..}
  = mUnless (samples == 0) $ B.char7 't' <> B.char7 '='
  <> prettyScientific (simple cfg) (fromRational mean) (Just $ sigmaLevel cfg * stdError a)
  <> B.char7 's'
  <> mUnless (hideDetails cfg)
  ( B.char7 ' '
    <> mUnless (samples <= 1)
    ( (if simple cfg then fromString "SD" else B.charUtf8 'σ')
      <> B.char7 '='
      <> prettyScientific (simple cfg) (100 * sigma a / fromRational mean) Nothing
      <> B.char7 '%' <> B.char7 ' '
    )
    <> B.char7 'n' <> B.char7 '='
    <> prettyNatural samples
  )

warmup :: IO ()
warmup = void . foldr1 (>=>) (replicate 10 step) . benchIO "warmup" $ pure ()

compareBench :: Config -> Double -> Benchmark -> Benchmark -> IO Ordering
compareBench cfg d x1 x2 = warmup *> fix go x1 x2
  where go h b1 b2 | oneOf ((<3) . samples) || oneOf ((<1) . informationOf) = next
                   | otherwise = case compareMeans cfg (analysis b1) (analysis b2) of
                       EQ | oneOf (relativeErrorAbove cfg (d/2)) -> next
                       r -> pure r
           where next | ((<=) `on` informationOf . analysis) b1 b2 = (`h` b2) =<< step b1
                      | otherwise = h b1 =<< step b2
                 oneOf f = f (analysis b1) || f (analysis b2)

relativeErrorAbove :: Config -> Double -> Analysis -> Bool
relativeErrorAbove cfg d a = d < sigmaLevel cfg * stdError a / fromRational (mean a)

compareMeans :: Config -> Analysis -> Analysis -> Ordering
compareMeans cfg a1 a2
  | f a1 a2 = LT
  | f a2 a1 = GT
  | otherwise = EQ
  where f x y = fromRational (mean x) + sigmaLevel cfg*stdError x < fromRational (mean y) - sigmaLevel cfg*stdError y

isEqualTo :: Benchmark -> Benchmark -> IO Bool
isEqualTo b1 b2 = (EQ==) <$> compareBench defaultConfig 0.01 b1 b2

isFasterThan :: Benchmark -> Benchmark -> IO Bool
isFasterThan b1 b2 = (LT==) <$> compareBench defaultConfig 0.01 b1 b2

prettyNatural :: Natural -> B.Builder
prettyNatural = go . fromIntegral
  where
    go x = case divMod x 1000 of
             (a,b) | a == 0 -> B.wordDec b
                   | b > 99 -> go a <> B.char7 ',' <> B.wordDec b
                   | b >  9 -> go a <> B.char7 ',' <> B.char7 '0' <> B.wordDec b
                   | otherwise -> go a <> B.char7 ',' <> B.char7 '0' <> B.char7 '0' <> B.wordDec b

prettyScientific :: Bool -> Double -> Maybe Double -> B.Builder
prettyScientific ascii x b = case floatToDigits 10 . min x <$> b of
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
    f e | ascii = B.char7 '*' <> B.intDec 10 <> mWhen (e/=2) (B.char7 '^' <> B.intDec (e-1))
        | otherwise = B.charUtf8 '·' <> B.intDec 10 <> mWhen (e/=2) (showE (e-1))

showE :: Integral a => a -> B.Builder
showE = fix go
  where go f n | n < 0 = B.charUtf8 '⁻' <> f (abs n)
               | n < 10 = B.charUtf8 $ "⁰¹²³⁴⁵⁶⁷⁸⁹" !! fromIntegral n
               | otherwise = uncurry ((<>) `on` f) $ divMod n 10

informationOf :: Analysis -> Double
informationOf Analysis{..} = fromRational mean ** 0.7 * fromIntegral samples

weightOf :: Analysis -> Natural
weightOf Analysis{..} = fromIntegral . max 1 . min samples . round . recip $ (fromRational mean :: Double) ** 0.7

{-# INLINE refineAnalysis #-}
refineAnalysis :: Analysis -> SystemTime -> SystemTime -> Analysis
refineAnalysis ana@Analysis{..} begin end = Analysis newSamples newSquaredWeights newMean newQFactor
  where
    newSamples = samples + weightOf ana
    newSquaredWeights = squaredWeights + weightOf ana*weightOf ana
    newMean = mean + diffWeight / fromIntegral newSamples
    newQFactor = qFactor + diffWeight * (time - newMean)
    diffWeight = fromIntegral (weightOf ana) * (time - mean)
    time = (toSeconds end - toSeconds begin) / fromIntegral (weightOf ana)
    toSeconds t = fromIntegral (systemSeconds t) + fromIntegral (systemNanoseconds t) / 1e9

sgrBuilder :: SGR -> B.Builder
sgrBuilder = (`csi'` 'm') . sgrToCode

csi' :: [Int] -> Char -> B.Builder
csi' (x:xs) b = B.char7 '\ESC' <> B.char7 '[' <> B.intDec x <> foldMap (\n -> B.char7 ';' <> B.intDec n) xs <> B.char7 b
csi' [] b = B.char7 '\ESC' <> B.char7 '[' <> B.char7 b

barBuilder :: Config -> Int -> Rational -> Double -> Double -> B.Builder
barBuilder cfg width m stdErr sd | simple cfg =
  B.char7 ' ' <> B.char7 ' ' <> B.string7 (replicate (pred valueLength) '=')
  <> B.string7 (replicate errorLength '<')
  <> mWhen (len * stdErr >= 0.20) (B.char7 '+')
  <> B.string7 (replicate errorLength '>')
  <> B.string7 (replicate sigmaLength '-')
                                 | otherwise =
  B.char7 ' ' <> B.char7 ' ' <> B.stringUtf8 (replicate (pred valueLength) '▀')
  <> sgrBuilder (SetColor Foreground Dull Magenta)
  <> B.stringUtf8 (replicate errorLength '▀')
  <> mWhen (len * stdErr >= 0.20)
           (sgrBuilder (SetColor Foreground Vivid Magenta) <> B.charUtf8 '▀')
  <> sgrBuilder (SetColor Foreground Dull Magenta)
  <> B.stringUtf8 (replicate errorLength '▀')
  <> sgrBuilder (SetColor Foreground Vivid Black)
  <> B.stringUtf8 (replicate sigmaLength '▔')
  <> sgrBuilder Reset

  where

    len = fromRational m * fromIntegral (width - 6) / 2
    valueLength = round len - errorLength
    errorLength = round $ len * stdErr
    sigmaLength = round (len * sd) - errorLength
