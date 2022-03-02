{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | basic measurement and callibration
module Main where

import Prelude
import Options.Applicative
import Perf
import Gauge
import qualified Data.Text.IO as Text
import Data.Text (Text)
import Control.DeepSeq
import qualified Data.Map.Strict as Map
import Data.FormatN
import qualified Data.Text as Text
import Control.Monad.State.Lazy
import qualified Data.List as List
import Data.Bool
import Data.Maybe
-- import Box.Csv hiding (header)
import NumHask.Space (quantile)

data RunType = RunExample | RunExamples | RunExampleIO | RunSums | RunLengths | RunGauge | RunNoOps | RunTicks deriving (Eq, Show)

data MeasureType = MeasureTime | MeasureSpace | MeasureSpaceTime | MeasureAllocation deriving (Eq, Show)

data Options = Options
  { optionRuns :: Int,
    optionLength :: Int,
    optionStatDType :: StatDType,
    optionRunType :: RunType,
    optionMeasureType :: MeasureType,
    optionsExample :: Example,
    optionsGolden :: Maybe FilePath,
    optionsRecord :: Bool,
    optionsRecordCheck :: Bool
  } deriving (Eq, Show)

parseRun :: Parser RunType
parseRun =
  flag' RunSums (long "sums" <> help "run on sum algorithms") <|>
  flag' RunLengths (long "lengths" <> help "run on length algorithms") <|>
  flag' RunExamples (long "examples" <> help "run on example algorithms") <|>
  flag' RunExample (long "example" <> help "run on the example algorithm") <|>
  flag' RunExampleIO (long "exampleIO" <> help "exampleIO test") <|>
  flag' RunNoOps (long "noops" <> help "noops test") <|>
  flag' RunTicks (long "ticks" <> help "tick test") <|>
  flag' RunGauge (long "gauge" <> help "gauge runs on exmaple for comparison") <|>
  pure RunExample

parseMeasure :: Parser MeasureType
parseMeasure =
  flag' MeasureTime (long "time" <> help "measure time performance") <|>
  flag' MeasureSpace (long "space" <> help "measure space performance") <|>
  flag' MeasureSpaceTime (long "spacetime" <> help "measure both space and time performance") <|>
  flag' MeasureAllocation (long "allocation" <> help "measure bytes allocated") <|>
  pure MeasureTime

-- | unification of the different measurements to being a list of doubles.
measureDs :: MeasureType -> Int -> Measure IO [[Double]]
measureDs mt n =
  case mt of
    MeasureTime -> fmap ((:[]) . fromIntegral) <$> times n
    MeasureSpace -> toMeasureN n (ssToList <$> space False)
    MeasureSpaceTime -> toMeasureN n ((\x y -> ssToList x <> [fromIntegral y]) <$> space False <*> stepTime)
    MeasureAllocation -> fmap ((:[]) . fromIntegral) <$> toMeasureN n (allocation False)

-- | unification of the different measurements to being a list of doubles.
measureLabels :: MeasureType -> [Text]
measureLabels mt =
  case mt of
    MeasureTime -> ["time"]
    MeasureSpace -> spaceLabels
    MeasureSpaceTime -> spaceLabels <> ["time"]
    MeasureAllocation -> ["allocation"]

options :: Parser Options
options = Options <$>
  option auto (value 1000 <> long "runs" <> short 'r' <> help "number of runs to perform") <*>
  option auto (value 1000 <> long "length" <> short 'l' <> help "length of list") <*>
  parseStatD <*>
  parseRun <*>
  parseMeasure <*>
  parseExample <*>
  optional (option str (long "golden" <> short 'g' <> help "golden file name")) <*>
  switch (long "record" <> short 'g' <> help "record the result to a golden file") <*>
  switch (long "check" <> short 'c' <> help "check versus a golden file")

opts :: ParserInfo Options
opts = info (options <**> helper)
  (fullDesc <> progDesc "perf benchmarking" <> header "basic perf callibration")

-- | * exampleIO

exampleIO :: (Semigroup t) => PerfT IO t ()
exampleIO = do
  txt <- fam "file read" (Text.readFile "src/Perf.hs")
  n <- fap "length" Text.length txt
  fam "print result" (Text.putStrLn $ "length of file is: " <> Text.pack (show n))

-- | * sums
-- | measure the various versions of a tick.
statTicks :: (NFData t, NFData b) => Text -> (t -> b) -> t -> Int -> StatDType -> StateT (Map.Map [Text] Double) IO ()
statTicks l f a n s = do
  addStat [l,"tick"] . statD s . fmap fromIntegral =<< lift (fst <$> multi tick n f a)
  addStat [l,"tickWHNF"] . statD s . fmap fromIntegral =<< lift (fst <$> multi tickWHNF n f a)
  addStat [l,"tickLazy"] . statD s . fmap fromIntegral =<< lift (fst <$> multi tickLazy n f a)
  addStat [l,"tickForce"] . statD s . fmap fromIntegral =<< lift (fst <$> multi tickForce n f a)
  addStat [l,"tickForceArgs"] . statD s . fmap fromIntegral =<< lift (fst <$> multi tickForceArgs n f a)
  addStat [l,"stepTime"] . statD s . fmap fromIntegral =<< lift (snd . head . Map.toList <$> execPerfT (toMeasureN n stepTime) (f |$| a))
  addStat [l,"times"] . statD s . fmap fromIntegral =<< lift (snd . head . Map.toList <$> execPerfT (times n) (f |$| a))

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

ordy :: Int -> [Text]
ordy f = zipWith (\x s -> (Text.pack . show) x <> s) [1..f] (["st", "nd", "rd"] <> repeat "th")

allStats :: Int -> Map.Map [Text] [[Double]] -> Map.Map [Text] [Double]
allStats f m = Map.fromList $ mconcat
  [ mconcat ((\(ks, xss) -> zipWith (\l xs -> (ks <> ["initial", l], xs)) (ordy f) xss) <$> mlist)
  , (\(ks, xss) -> (ks <> ["best"], quantile 0.1 <$> List.transpose xss)) <$> mlist
  , (\(ks, xss) -> (ks <> ["median"], quantile 0.5 <$> List.transpose xss)) <$> mlist
  , (\(ks, xss) -> (ks <> ["average"], av <$> List.transpose xss)) <$> mlist
  ]
  where
    mlist = Map.toList m
    av xs = sum xs / (fromIntegral . length $ xs)

-- * org-mode formatting
outercalate :: Text -> [Text] -> Text
outercalate c ts = c <> Text.intercalate c ts <> c

printOrgHeader :: Map.Map [Text] a -> [Text] -> IO ()
printOrgHeader m ts = do
  Text.putStrLn $ outercalate "|" ((("label" <>) . Text.pack . show <$> [1..labelCols]) <> ts)
  Text.putStrLn $ outercalate "|" (replicate (labelCols+1) "---")
  pure ()
    where
      labelCols = maximum $ length <$> Map.keys m

printOrg :: Map.Map [Text] Text -> IO ()
printOrg m = do
  printOrgHeader m ["results"]
  _ <- Map.traverseWithKey (\k a -> Text.putStrLn (outercalate "|" (k <> [a]))) m
  pure ()

unlistify :: Map.Map [Text] [a] -> Map.Map [Text] a
unlistify m = Map.fromList $ mconcat $ (\(ks,vs) -> (\(v,i) -> (ks <> [Text.pack (show i)], v)) <$> zip vs [(0::Int)..]) <$> Map.toList m

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

prettyOrgSpace :: SpaceStats -> Text
prettyOrgSpace (SpaceStats x1 x2 x3 x4 x5) =
  Text.intercalate "|"
  [expt (Just 3) (fromIntegral x1),
   fixed (Just 0) (fromIntegral x2),
   expt (Just 3) (fromIntegral x3),
   expt (Just 3) (fromIntegral x4),
   expt (Just 3) (fromIntegral x5)]

printOrgSpace :: Map.Map [Text] SpaceStats -> IO ()
printOrgSpace m = do
  printOrgHeader m spaceLabels
  void $ Map.traverseWithKey (\k a -> Text.putStrLn ("|" <> Text.intercalate "|" k <> "|" <> prettyOrgSpace a <> "|")) m

printOrgSpaceTime :: Map.Map [Text] (Cycles, SpaceStats) -> IO ()
printOrgSpaceTime m = do
  printOrgHeader m ("time":spaceLabels)
  void $ Map.traverseWithKey (\k (c,s) -> Text.putStrLn (outercalate "|"  (k <> [expt (Just 3) (fromIntegral c), prettyOrgSpace s]))) m

-- | * gauge experiment
testGauge :: (NFData b) =>
  Text -> (a -> b) -> a -> IO ()
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

rioOrg :: Maybe FilePath -> Maybe FilePath -> StatDType -> [Text] -> Map.Map [Text] [[Double]] -> IO ()
rioOrg check record s labels m = do
    case check of
      Nothing -> printOrg (expt (Just 3) <$> m')
      Just fp -> degradePrint defaultDegradeConfig fp m'
    mapM_ (`writeResult` m') record
    where
      m' = Map.fromList $ mconcat $ (\(ks,xss) -> zipWith (\x l -> (ks <> [l], statD s x)) (List.transpose xss) labels) <$> Map.toList m

rioOrgRaw :: Maybe FilePath -> Maybe FilePath -> [Text] -> Map.Map [Text] [Double] -> IO ()
rioOrgRaw check record labels m = do
    case check of
      Nothing -> printOrg (expt (Just 3) <$> m')
      Just fp -> degradePrint defaultDegradeConfig fp m'
    mapM_ (`writeResult` m') record
    where
      m' = Map.fromList $ mconcat $ (\(ks,xss) -> zipWith (\x l -> (ks <> [l], x)) xss labels) <$> Map.toList m

main :: IO ()
main = do
  o <- execParser opts
  let !n = optionRuns o
  let !l = optionLength o
  let s = optionStatDType o
  let a = optionsExample o
  let r = optionRunType o
  let mt = optionMeasureType o
  let fp = fromMaybe ("other/" <> show r <> ".csv") (optionsGolden o)
  let record = bool Nothing (Just fp) (optionsRecord o)
  let check = bool Nothing (Just fp) (optionsRecordCheck o)
  let rio = rioOrg check record s

  case r of
    RunExample-> do
      m <- execPerfT (measureDs mt n) $ testExample (examplePattern a l)
      rio (measureLabels mt) (Map.mapKeys (:[]) m)

    RunExamples-> do
      m <- statExamples n l (measureDs mt)
      rio (measureLabels mt) (Map.mapKeys (:[]) m)

    RunExampleIO -> do
      m1 <- execPerfT (measureDs mt n) exampleIO
      (_, (m', m2)) <- outer "outer-total" (measureDs mt n) (measureDs mt n) exampleIO
      let ms = mconcat [Map.mapKeys (\x -> ["normal", x]) m1, Map.mapKeys (\x -> ["outer", x]) (m2 <> m')]
      rio (measureLabels mt) ms

    RunSums-> do
      m <- statSums n l (measureDs mt)
      rio (measureLabels mt) (Map.mapKeys (:[]) m)

    RunLengths-> do
      m <- statLengths n l (measureDs mt)
      rio (measureLabels mt) (Map.mapKeys (:[]) m)

    RunNoOps -> do
      m <- perfNoOps (measureDs mt n)
      rioOrgRaw check record (measureLabels mt) (allStats 4 (Map.mapKeys (:[]) m))

    RunTicks -> do
      m <- statTicksSums n l s
      printOrg2DTranspose (fmap (expt (Just 3)) m)

    RunGauge -> do
      mapM_ testGaugeExample ((`examplePattern` l) <$> allExamples)
