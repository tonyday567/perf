{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- |

module Perf.Stats
  ( averageD,
    medianD,
    tenthD,
    averageSecsD,
    StatDType (..),
    statD,
    parseStatD,

    -- stat reporting
    addStat,
    readStats,
    writeStats,
    printOrg,
    printOrg2D,
    printOrg2DTranspose,

    -- * algos
    -- no ops
    runNoOps,

    -- sum algorithms
    testSum,
    testTickBySum,
    runAllSums,
    testExample,
  ) where

import Perf.Types
import Perf.Time
import Perf.Algos
import qualified Data.Text.IO as Text
import Data.Text (Text)
import NumHask.Space (quantile)
import Control.DeepSeq
import Options.Applicative
import qualified Data.Map.Strict as Map
import Control.Monad.State.Lazy
import Box.Csv
import Box
import qualified Data.Text as Text
import qualified Data.List as List
import Data.FormatN

-- convert a list (of Integrals) to a double stat
medianD :: (Integral a) => [a] -> Double
medianD = quantile 0.5 . fmap Prelude.fromIntegral

averageD :: (Integral a) => [a] -> Double
averageD xs = (fromIntegral . Prelude.toInteger . sum $ xs) / (fromIntegral . length $ xs)

tenthD :: (Integral a) => [a] -> Double
tenthD = quantile 0.1 . fmap Prelude.fromIntegral

averageSecsD :: (Integral a) => [a] -> Double
averageSecsD xs = (fromIntegral . Prelude.toInteger . sum $ xs) / (fromIntegral . length $ xs) / 2.5e9

data StatDType = StatAverage | StatMedian | StatBest | StatSecs deriving (Eq, Show)

statD :: StatDType -> (Integral a) => [a] -> Double
statD StatBest = tenthD
statD StatMedian = medianD
statD StatAverage = averageD
statD StatSecs = averageSecsD

parseStatD :: Parser StatDType
parseStatD =
  flag' StatBest (long "best" <> help "report upper decile") <|>
  flag' StatMedian (long "median" <> help "report median") <|>
  flag' StatAverage (long "average" <> help "report average") <|>
  flag' StatSecs (long "averagesecs" <> help "report average in seconds") <|>
  pure StatAverage

-- stat reporting
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

-- | run the various incarnations of tick.
testTick :: (NFData t, NFData b) => Text -> (t -> b) -> t -> Int -> StatDType -> StateT (Map.Map [Text] Double) IO ()
testTick l f a n s = do
  addStat [l,"tick"] . statD s =<< lift (fst <$> multi tick n f a)
  addStat [l,"tickWHNF"] . statD s =<< lift (fst <$> multi tickWHNF n f a)
  addStat [l,"tickLazy"] . statD s =<< lift (fst <$> multi tickLazy n f a)
  addStat [l,"tickForce"] . statD s =<< lift (fst <$> multi tickForce n f a)
  addStat [l,"tickForceArgs"] . statD s =<< lift (fst <$> multi tickForceArgs n f a)
  addStat [l,"stepTime"] . statD s =<< lift (snd . head . Map.toList <$> execPerfT (toMeasureN n stepTime) (f |$| a))
  addStat [l,"times"] . statD s =<< lift (snd . head . Map.toList <$> execPerfT (times n) (f |$| a))

testTickBySum :: Int -> Int -> StatDType -> IO (Map.Map [Text] Double)
testTickBySum n l s = flip execStateT Map.empty $ mapM_ (\x -> testSumTick x n s) (allSums l)

testSumTick :: SumPattern Int -> Int -> StatDType -> StateT (Map.Map [Text] Double) IO ()
testSumTick (SumFuse label f a) n s = testTick label f a n s
testSumTick (SumFusePoly label f a) n s = testTick label f a n s
testSumTick (SumPoly label f a) n s = testTick label f a n s
testSumTick (SumMono label f a) n s = testTick label f a n s

testSum :: (Semigroup a, MonadIO m) => SumPattern Int -> PerfT m a Int
testSum (SumFuse label f a) = fap label f a
testSum (SumFusePoly label f a) = fap label f a
testSum (SumMono label f a) = fap label f a
testSum (SumPoly label f a) = fap label f a

runAllSums :: (MonadIO m) => Int -> Int -> (Int -> Measure m [a]) -> m (Map.Map Text [a])
runAllSums n l m = execPerfT (m n) $ mapM_ testSum (allSums l)

testExample :: (Semigroup a, MonadIO m) => ExamplePattern Int -> PerfT m a ()
testExample (PatternSumFuse label f a) = void $ fap label f a
testExample (PatternSum label f a) = void $ fap label f a
testExample (PatternLengthF label f a) = void $ fap label f a
testExample (PatternConstFuse label f a) = void $ fap label f a
testExample (PatternMapInc label f a) = void $ fap label f a

noOps :: Int -> Maybe FilePath -> IO (Map.Map Text [Word64])
noOps n fp = do
    m <- execPerfT (times n) $ do
      liftIO $ warmup 1000
      fap "fap times" (const ()) ()
      fam "fam times" (pure ())
    case fp of
      Nothing -> pure ()
      Just fp' -> writeFile fp' (show m)
    pure m

runNoOps :: Maybe FilePath -> Int -> Int -> IO (Map.Map [Text] Text)
runNoOps fp f n = flip execStateT Map.empty $ do
  m <- lift (noOps n fp)
  mapM_ (addStat ["first " <> Text.pack (show f), " faps"] . Text.pack . show . take f) (Map.lookup "fap times" m)
  mapM_ (addStat ["first " <> Text.pack (show f), " fams"] . Text.pack . show . take f) (Map.lookup "fam times" m)
  mapM_ (addStat ["best","faps"] . expt (Just 3) . tenthD) (Map.lookup "fap times" m)
  mapM_ (addStat ["best","fams"] . expt (Just 3) . tenthD) (Map.lookup "fam times" m)
  mapM_ (addStat ["median","faps"] . expt (Just 3) . medianD) (Map.lookup "fap times" m)
  mapM_ (addStat ["median","fams"] . expt (Just 3) . medianD) (Map.lookup "fam times" m)
  mapM_ (addStat ["average","faps"] . expt (Just 3) . averageD) (Map.lookup "fap times" m)
  mapM_ (addStat ["average","fams"] . expt (Just 3) . averageD) (Map.lookup "fam times" m)

printOrg :: Map.Map [Text] Text -> IO ()
printOrg m = do
    Text.putStrLn "|stat|result|"
    Text.putStrLn "|---|---|"
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
