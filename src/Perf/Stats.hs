{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- |

module Perf.Stats
  ( average,
    median,
    tenth,
    averageSecs,
    StatType (..),
    stat,
    statD,
    parseStat,

    -- no ops
    runNoOps,
    testNoOps,
    addStat,
    readStats,
    writeStats,
    printOrg,
    printOrg2D,
    printOrg2DTranspose,

    -- * algos
    testSum,
    testSum',
    testSums,
    testAlgos,
    testStyleBySum,
  ) where

import Perf
import qualified Data.Text.IO as Text
import Data.Text (Text)
import Data.FormatN
import NumHask.Space (quantile)
import Perf.Algos
import Control.DeepSeq
import Options.Applicative
import qualified Data.Map.Strict as Map
import Control.Monad.State.Lazy
import Box.Csv
import Box
import qualified Data.Text as Text
import qualified Data.List as List

median :: (Integral a) => [a] -> Text
median = expt (Just 3) . quantile 0.5 . fmap Prelude.fromIntegral

medianD :: (Integral a) => [a] -> Double
medianD = quantile 0.5 . fmap Prelude.fromIntegral

average :: (Integral a) => [a] -> Text
average = expt (Just 3) . (\xs -> (fromIntegral . Prelude.toInteger . sum $ xs) / (fromIntegral . length $ xs))

averageD :: (Integral a) => [a] -> Double
averageD xs = (fromIntegral . Prelude.toInteger . sum $ xs) / (fromIntegral . length $ xs)

tenth :: (Integral a) => [a] -> Text
tenth = expt (Just 3) . quantile 0.1 . fmap Prelude.fromIntegral

tenthD :: (Integral a) => [a] -> Double
tenthD = quantile 0.1 . fmap Prelude.fromIntegral

data StatType = StatAverage | StatMedian | StatBest | StatSecs deriving (Eq, Show)

averageSecs :: (Integral a) => [a] -> Text
averageSecs = expt (Just 3) . (\xs -> (fromIntegral . Prelude.toInteger . sum $ xs) / (fromIntegral . length $ xs) / 2.5e9)

averageSecsD :: (Integral a) => [a] -> Double
averageSecsD xs = (fromIntegral . Prelude.toInteger . sum $ xs) / (fromIntegral . length $ xs) / 2.5e9

stat :: StatType -> (Integral a) => [a] -> Text
stat StatBest = tenth
stat StatMedian = median
stat StatAverage = average
stat StatSecs = averageSecs

statD :: StatType -> (Integral a) => [a] -> Double
statD StatBest = tenthD
statD StatMedian = medianD
statD StatAverage = averageD
statD StatSecs = averageSecsD

parseStat :: Parser StatType
parseStat =
  flag' StatBest (long "best" <> help "report upper decile") <|>
  flag' StatMedian (long "median" <> help "report median") <|>
  flag' StatAverage (long "average" <> help "report average") <|>
  flag' StatSecs (long "averagesecs" <> help "report average in seconds") <|>
  pure StatAverage

addStat :: (Ord k, Monad m) => k -> s -> StateT (Map.Map k s) m ()
addStat label s = do
  modify (Map.insert label s)

csvFile :: CsvConfig
csvFile = CsvConfig "./other/default.csv" ',' NoHeader

writeStats :: FilePath -> (a -> Text) -> Map.Map [Text] a -> IO ()
writeStats fp f m =
  glue <$> rowCommitter (csvFile { file = fp }) (\(l,v) -> l <> [f v]) <*|> qList (Map.toList m)

readStats :: FilePath -> IO (Map.Map [Text] Text)
readStats fp = do
  r <- runCsv (csvFile { file = fp  }) fields
  pure $ Map.fromList [(init x, last x) | (Right x) <- r]

testCountStyles :: (NFData t, NFData b) => Text -> (t -> b) -> t -> Int -> StatType -> StateT (Map.Map [Text] Text) IO ()
testCountStyles l f a n s = do
  addStat [l,"count"] . stat s =<< lift (fst <$> multiple count n f a)
  addStat [l,"countWHNF"] . stat s =<< lift (fst <$> multiple countWHNF n f a)
  addStat [l,"countLazy"] . stat s =<< lift (fst <$> multiple countLazy n f a)
  addStat [l,"countForce"] . stat s =<< lift (fst <$> multiple countForce n f a)
  addStat [l,"countForceArgs"] . stat s =<< lift (fst <$> multiple countForceArgs n f a)
  addStat [l,"cycles"] . stat s =<< lift (snd . head . Map.toList <$> execPerfT (ticks n) (f |$| a))

testAlgos :: AlgoApplication Int -> Int -> StatType -> StateT (Map.Map [Text] Text) IO ()
testAlgos (ApplicationFuseSum label f a) n s = testCountStyles label f a n s
testAlgos (ApplicationFuseConst label f a) n s = testCountStyles label f a n s
testAlgos (ApplicationRecSum label f a) n s = testCountStyles label f a n s
testAlgos (ApplicationMonoSum label f a) n s = testCountStyles label f a n s
testAlgos (ApplicationPolySum label f a) n s = testCountStyles label f a n s
testAlgos (ApplicationLambdaSum label f a) n s = testCountStyles label f a n s

testSum :: (Semigroup a, MonadIO m) => SumAlgo Int -> PerfT m a Int
testSum (SumFuse label f a) = fap label f a
testSum (SumMono label f a) = fap label f a
testSum (SumPoly label f a) = fap label f a
testSum (SumLambda label f a) = fap label f a

testSums :: (MonadIO m) => Int -> Int -> StepMeasure m a -> m (Map.Map Text [a])
testSums n l m = execPerfT (toMeasureN n m) $ mapM_ testSum (allSums l)

testStyleBySum :: Int -> Int -> StatType -> IO (Map.Map [Text] Text)
testStyleBySum n l s = flip execStateT Map.empty $ mapM_ (\x -> testSum' x n s) (allSums l)

testSum' :: SumAlgo Int -> Int -> StatType -> StateT (Map.Map [Text] Text) IO ()
testSum' (SumFuse label f a) n s = testCountStyles label f a n s
testSum' (SumMono label f a) n s = testCountStyles label f a n s
testSum' (SumPoly label f a) n s = testCountStyles label f a n s
testSum' (SumLambda label f a) n s = testCountStyles label f a n s

runNoOps :: Int -> Maybe FilePath -> IO (Map.Map Text [Word64])
runNoOps n fp = do
    m <- execPerfT (ticks n) $ do
      liftIO $ warmup 1000
      fap "fap cycles'" (const ()) ()
      fam "fam cycles'" (pure ())
    case fp of
      Nothing -> pure ()
      Just fp' -> writeFile fp' (show m)
    pure m

testNoOps :: Maybe FilePath -> Int -> Int -> IO (Map.Map [Text] Text)
testNoOps fp f n = flip execStateT Map.empty $ do
  m <- lift (runNoOps n fp)
  mapM_ (addStat ["first " <> Text.pack (show f), " faps"] . Text.pack . show . take f) (Map.lookup "fap cycles'" m)
  mapM_ (addStat ["first " <> Text.pack (show f), " fams"] . Text.pack . show . take f) (Map.lookup "fam cycles'" m)
  mapM_ (addStat ["best","faps"] . tenth) (Map.lookup "fap cycles'" m)
  mapM_ (addStat ["best","fams"] . tenth) (Map.lookup "fam cycles'" m)
  mapM_ (addStat ["median","faps"] . median) (Map.lookup "fap cycles'" m)
  mapM_ (addStat ["median","fams"] . median) (Map.lookup "fam cycles'" m)
  mapM_ (addStat ["average","faps"] . average) (Map.lookup "fap cycles'" m)
  mapM_ (addStat ["average","fams"] . average) (Map.lookup "fam cycles'" m)

printOrg :: Map.Map [Text] Text -> IO ()
printOrg m = do
    Text.putStrLn "|stat|result|"
    _ <- Map.traverseWithKey (\k a -> Text.putStrLn ("|" <> Text.intercalate "|" k <> "|" <> a <> "|")) m
    pure ()

printOrg2D :: Map.Map [Text] Text -> IO ()
printOrg2D m = do
    let rs = List.nub ((List.!! 0) . fst <$> Map.toList m)
    let cs = List.nub ((List.!! 1) . fst <$> Map.toList m)
    Text.putStrLn ("||" <> Text.intercalate "|" rs <> "|")
    sequence_ $
      (\c -> Text.putStrLn
        ("|" <> c <> "|" <>
          Text.intercalate "|" ((\r -> m Map.! [r,c]) <$> rs) <> "|")) <$> cs

printOrg2DTranspose :: Map.Map [Text] Text -> IO ()
printOrg2DTranspose m = do
    let rs = List.nub ((List.!! 1) . fst <$> Map.toList m)
    let cs = List.nub ((List.!! 0) . fst <$> Map.toList m)
    Text.putStrLn ("||" <> Text.intercalate "|" rs <> "|")
    sequence_ $
      (\c -> Text.putStrLn
        ("|" <> c <> "|" <>
          Text.intercalate "|" ((\r -> m Map.! [c,r]) <$> rs) <> "|")) <$> cs
