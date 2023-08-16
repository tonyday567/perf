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

data RunType = RunExample | RunExamples | RunNub | RunExampleIO | RunSums | RunLengths | RunGauge | RunNoOps | RunTicks deriving (Eq, Show)

data ExploreOptions = ExploreOptions
  { exploreReportOptions :: ReportOptions,
    exploreRun :: RunType,
    exploreLength :: Int,
    exploreExample :: Example
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

exploreOptions :: Parser ExploreOptions
exploreOptions =
  ExploreOptions
    <$> parseReportOptions
    <*> parseRun
    <*> option auto (value 1000 <> long "length" <> short 'l' <> help "length of list")
    <*> parseExample

exploreOpts :: ParserInfo ExploreOptions
exploreOpts =
  info
    (exploreOptions <**> helper)
    (fullDesc <> progDesc "perf exploration" <> header "examples of perf usage")

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

main :: IO ()
main = do
  o <- execParser exploreOpts
  let repOptions = exploreReportOptions o
  let n = reportN repOptions
  let s = reportStatDType repOptions
  let mt = reportMeasureType repOptions
  let !l = exploreLength o
  let a = exploreExample o
  let r = exploreRun o

  case r of
    RunNub -> do
      reportMainWith repOptions (show r) (ffap "nub" nub [1 .. l])
    RunExample -> do
      reportMainWith repOptions (intercalate "-" [show r, show a, show l]) $
        testExample (examplePattern a l)
    RunExamples -> do
      reportMainWith repOptions (intercalate "-" [show r, show l])
        (statExamples l)
    RunExampleIO -> do
      m1 <- execPerfT (measureDs mt 1) exampleIO
      (_, (m', m2)) <- outer "outer-total" (measureDs mt 1) (measureDs mt 1) exampleIO
      let ms = mconcat [Map.mapKeys (\x -> ["normal", x]) m1, Map.mapKeys (\x -> ["outer", x]) (m2 <> m')]
      putStrLn ""
      let o' = replaceDefaultFilePath (intercalate "-" [show r, show mt, show s]) repOptions
      report o' (fmap (statD s) <$> ms)
    RunSums -> do
      m <- statSums n l (measureDs mt)
      let o' = replaceDefaultFilePath (intercalate "-" [show r, show mt, show n, show l, show s]) repOptions
      report o' (statify s m)
    RunLengths -> do
      m <- statLengths n l (measureDs mt)
      let o' = replaceDefaultFilePath (intercalate "-" [show r, show mt, show n, show l, show s]) repOptions
      report o' (statify s m)
    RunNoOps -> do
      m <- perfNoOps (measureDs mt n)
      let o' = replaceDefaultFilePath (intercalate "-" [show r, show mt, show n]) repOptions
      report o' (allStats 4 (Map.mapKeys (: []) m))
    RunTicks -> do
      m <- statTicksSums n l s
      reportOrg2D (fmap (expt (Just 3)) m)
    RunGauge -> do
      mapM_ testGaugeExample ((`examplePattern` l) <$> allExamples)
