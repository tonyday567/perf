{-# LANGUAGE OverloadedStrings #-}

-- | basic measurement and callibration
module Main where

import Control.DeepSeq
import Control.Monad
import Control.Monad.State.Lazy
import Data.FormatN
import Data.List (intercalate, nub)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Gauge
import Options.Applicative
import Perf
import Prelude
import System.Exit

data RunType = RunExample | RunExamples | RunNub | RunExampleIO | RunSums | RunLengths | RunGauge | RunNoOps | RunTicks deriving (Eq, Show)

data Options = Options
  { optionN :: Int,
    optionLength :: Int,
    optionStatDType :: StatDType,
    optionRunType :: RunType,
    optionMeasureType :: MeasureType,
    optionExample :: Example,
    optionGolden :: Golden,
    optionReportConfig :: ReportConfig,
    optionRawStats :: Bool
  }
  deriving (Eq, Show)

parseRun :: Parser RunType
parseRun =
  flag' RunSums (long "sums" <> help "run on sum algorithms")
    <|> flag' RunLengths (long "lengths" <> help "run on length algorithms")
    <|> flag' RunNub (long "nub" <> help "nub test")
    <|> flag' RunExamples (long "examples" <> help "run on example algorithms")
    <|> flag' RunExample (long "example" <> help "run on the example algorithm")
    <|> flag' RunExampleIO (long "exampleIO" <> help "exampleIO test")
    <|> flag' RunNoOps (long "noops" <> help "noops test")
    <|> flag' RunTicks (long "ticks" <> help "tick test")
    <|> flag' RunGauge (long "gauge" <> help "gauge runs on exmaple for comparison")
    <|> pure RunExample

options :: Parser Options
options =
  Options
    <$> option auto (value 1000 <> long "runs" <> short 'n' <> help "number of runs to perform")
    <*> option auto (value 1000 <> long "length" <> short 'l' <> help "length of list")
    <*> parseStatD
    <*> parseRun
    <*> parseMeasure
    <*> parseExample
    <*> parseGolden "golden"
    <*> parseReportConfig defaultReportConfig
    <*> switch (long "raw" <> short 'w' <> help "write raw statistics to file")

opts :: ParserInfo Options
opts =
  info
    (options <**> helper)
    (fullDesc <> progDesc "perf benchmarking" <> header "basic perf callibration")

-- | * exampleIO
exampleIO :: (Semigroup t) => PerfT IO t ()
exampleIO = do
  txt <- fam "file-read" (Text.readFile "src/Perf.hs")
  n <- fap "length" Text.length txt
  fam "print-result" (Text.putStrLn $ "length of file is: " <> Text.pack (show n))

-- | * sums
-- | measure the various versions of a tick.
statTicks :: (NFData t, NFData b) => Text -> (t -> b) -> t -> Int -> StatDType -> StateT (Map.Map [Text] Double) IO ()
statTicks l f a n s = do
  addStat [l, "tick"] . statD s . fmap fromIntegral =<< lift (fst <$> multi tick n f a)
  addStat [l, "tickWHNF"] . statD s . fmap fromIntegral =<< lift (fst <$> multi tickWHNF n f a)
  addStat [l, "tickLazy"] . statD s . fmap fromIntegral =<< lift (fst <$> multi tickLazy n f a)
  addStat [l, "tickForce"] . statD s . fmap fromIntegral =<< lift (fst <$> multi tickForce n f a)
  addStat [l, "tickForceArgs"] . statD s . fmap fromIntegral =<< lift (fst <$> multi tickForceArgs n f a)
  addStat [l, "stepTime"] . statD s . fmap fromIntegral =<< lift (snd . head . Map.toList <$> execPerfT (toMeasureN n stepTime) (f |$| a))
  addStat [l, "times"] . statD s . fmap fromIntegral =<< lift (snd . head . Map.toList <$> execPerfT (times n) (f |$| a))

statTicksSum :: (NFData b, Enum b, Num b) => SumPattern b -> Int -> StatDType -> StateT (Map.Map [Text] Double) IO ()
statTicksSum (SumFuse label f a) n s = statTicks label f a n s
statTicksSum (SumFusePoly label f a) n s = statTicks label f a n s
statTicksSum (SumPoly label f a) n s = statTicks label f a n s
statTicksSum (SumMono label f a) n s = statTicks label f a n s

statTicksSums :: Int -> Int -> StatDType -> IO (Map.Map [Text] Double)
statTicksSums n l s = flip execStateT Map.empty $ mapM_ (\x -> statTicksSum x n s) (allSums l)

-- * no-op testing

perfNoOps :: (Semigroup a) => Measure IO a -> IO (Map.Map Text a)
perfNoOps meas =
  execPerfT meas $ do
    liftIO $ warmup 1000
    fap "const" (const ()) ()
    fam "pure" (pure ())

-- | * gauge experiment
testGauge ::
  (NFData b) =>
  Text ->
  (a -> b) ->
  a ->
  IO ()
testGauge label f a = do
  Text.putStrLn label
  benchmarkWith defaultConfig (whnf f a)
  benchmarkWith defaultConfig (nf f a)

testGaugeExample :: ExamplePattern Int -> IO ()
testGaugeExample (PatternSumFuse label f a) = testGauge label f a
testGaugeExample (PatternSum label f a) = testGauge label f a
testGaugeExample (PatternLengthF label f a) = testGauge label f a
testGaugeExample (PatternConstFuse label f a) = testGauge label f a
testGaugeExample (PatternMapInc label f a) = testGauge label f a
testGaugeExample (PatternNoOp label f a) = testGauge label f a

main :: IO ExitCode
main = do
  o <- execParser opts
  let !n = optionN o
  let !l = optionLength o
  let s = optionStatDType o
  let a = optionExample o
  let r = optionRunType o
  let mt = optionMeasureType o
  let gold = goldenFromOptions [show r, show n, show l, show mt] (optionGolden o)
  let w = optionRawStats o
  let raw =
        "other/"
          <> intercalate "-" [show r, show n, show l, show mt]
          <> ".map"
  let cfg = optionReportConfig o

  case r of
    RunNub -> do
      m <- execPerfT (measureDs mt n) $ void $ ffap "nub" nub [1 .. l]
      when w (writeFile raw (show m))
      report cfg gold (measureLabels mt) (statify s m)
    RunExample -> do
      m <- execPerfT (measureDs mt n) $ testExample (examplePattern a l)
      when w (writeFile raw (show m))
      report cfg gold (measureLabels mt) (statify s m)
    RunExamples -> do
      m <- statExamples n l (measureDs mt)
      when w (writeFile raw (show m))
      report cfg gold (measureLabels mt) (statify s m)
    RunExampleIO -> do
      m1 <- execPerfT (measureDs mt 1) exampleIO
      (_, (m', m2)) <- outer "outer-total" (measureDs mt 1) (measureDs mt 1) exampleIO
      let ms = mconcat [Map.mapKeys (\x -> ["normal", x]) m1, Map.mapKeys (\x -> ["outer", x]) (m2 <> m')]
      putStrLn ""
      when w (writeFile raw (show ms))
      report cfg gold (measureLabels mt) (fmap (statD s) <$> ms)
    RunSums -> do
      m <- statSums n l (measureDs mt)
      when w (writeFile raw (show m))
      report cfg gold (measureLabels mt) (statify s m)
    RunLengths -> do
      m <- statLengths n l (measureDs mt)
      when w (writeFile raw (show m))
      report cfg gold (measureLabels mt) (statify s m)
    RunNoOps -> do
      m <- perfNoOps (measureDs mt n)
      when w (writeFile raw (show m))
      report cfg gold (measureLabels mt) (allStats 4 (Map.mapKeys (: []) m))
    RunTicks -> do
      m <- statTicksSums n l s
      when w (writeFile raw (show m))
      reportOrg2D (fmap (expt (Just 3)) m) >> pure ExitSuccess
    RunGauge -> do
      mapM_ testGaugeExample ((`examplePattern` l) <$> allExamples) >> pure ExitSuccess
