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

data RunType = RunDefault | RunExamples | RunExampleIO | RunSums | RunGauge | RunNoOp | RunTicks | RunTime | RunSpace | RunSpaceTime deriving (Eq, Show)

data Options = Options
  { optionRuns :: Int,
    optionLength :: Int,
    optionStatDType :: StatDType,
    optionRunType :: RunType,
    optionsExample :: Example,
    optionsGolden :: Maybe FilePath,
    optionsRecord :: Bool,
    optionsRecordCheck :: Bool
  } deriving (Eq, Show)

parseRun :: Parser RunType
parseRun =
  flag' RunDefault (long "default" <> help "default measurement (polySum)") <|>
  flag' RunSums (long "sums" <> help "sums") <|>
  flag' RunExamples (long "examples" <> help "examples") <|>
  flag' RunExampleIO (long "exampleIO" <> help "exampleIO") <|>
  flag' RunNoOp (long "noop" <> help "no-ops") <|>
  flag' RunTicks (long "ticks" <> help "tick types") <|>
  flag' RunGauge (long "gauge" <> help "gauge comparison") <|>
  flag' RunTime (long "time" <> help "time stats") <|>
  flag' RunSpace (long "space" <> help "space stats") <|>
  flag' RunSpaceTime (long "spacetime" <> help "space and time stats") <|>
  pure RunDefault

options :: Parser Options
options = Options <$>
  option auto (value 1000 <> long "runs" <> short 'r' <> help "number of runs to perform") <*>
  option auto (value 1000 <> long "length" <> short 'l' <> help "length of list") <*>
  parseStatD <*>
  parseRun <*>
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
  addStat [l,"tick"] . statD s =<< lift (fst <$> multi tick n f a)
  addStat [l,"tickWHNF"] . statD s =<< lift (fst <$> multi tickWHNF n f a)
  addStat [l,"tickLazy"] . statD s =<< lift (fst <$> multi tickLazy n f a)
  addStat [l,"tickForce"] . statD s =<< lift (fst <$> multi tickForce n f a)
  addStat [l,"tickForceArgs"] . statD s =<< lift (fst <$> multi tickForceArgs n f a)
  addStat [l,"stepTime"] . statD s =<< lift (snd . head . Map.toList <$> execPerfT (toMeasureN n stepTime) (f |$| a))
  addStat [l,"times"] . statD s =<< lift (snd . head . Map.toList <$> execPerfT (times n) (f |$| a))

statTicksSum :: (NFData b, Enum b, Num b) => SumPattern b -> Int -> StatDType -> StateT (Map.Map [Text] Double) IO ()
statTicksSum (SumFuse label f a) n s = statTicks label f a n s
statTicksSum (SumFusePoly label f a) n s = statTicks label f a n s
statTicksSum (SumPoly label f a) n s = statTicks label f a n s
statTicksSum (SumMono label f a) n s = statTicks label f a n s

statTicksSums :: Int -> Int -> StatDType -> IO (Map.Map [Text] Double)
statTicksSums n l s = flip execStateT Map.empty $ mapM_ (\x -> statTicksSum x n s) (allSums l)

-- * no-op testing
statNoOp :: Int -> Maybe FilePath -> IO (Map.Map Text [Cycles])
statNoOp n fp = do
    m <- execPerfT (times n) $ do
      liftIO $ warmup 1000
      fap "fap times" (const ()) ()
      fam "fam times" (pure ())
    case fp of
      Nothing -> pure ()
      Just fp' -> writeFile fp' (show m)
    pure m

statNoOps :: Maybe FilePath -> Int -> Int -> IO (Map.Map [Text] Text)
statNoOps fp f n = flip execStateT Map.empty $ do
  m <- lift (statNoOp n fp)
  mapM_ (addStat ["first " <> (fixed (Just 0) . fromIntegral) f, "faps"] . Text.intercalate " " . fmap (fixed Nothing . fromIntegral) . take f) (Map.lookup "fap times" m)
  mapM_ (addStat ["first " <> (fixed (Just 0) . fromIntegral) f, "fams"] . Text.intercalate " " . fmap (fixed Nothing . fromIntegral) . take f) (Map.lookup "fam times" m)
  mapM_ (addStat ["best","faps"] . expt (Just 3) . tenthD) (Map.lookup "fap times" m)
  mapM_ (addStat ["best","fams"] . expt (Just 3) . tenthD) (Map.lookup "fam times" m)
  mapM_ (addStat ["median","faps"] . expt (Just 3) . medianD) (Map.lookup "fap times" m)
  mapM_ (addStat ["median","fams"] . expt (Just 3) . medianD) (Map.lookup "fam times" m)
  mapM_ (addStat ["average","faps"] . expt (Just 3) . averageD) (Map.lookup "fap times" m)
  mapM_ (addStat ["average","fams"] . expt (Just 3) . averageD) (Map.lookup "fam times" m)


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

recordSpaceStats :: FilePath -> (a -> b) -> a -> Int -> IO ()
recordSpaceStats fp f a n = do
  xs <- execPerfT (toMeasureN n (space False)) (f |$| a)
  writeFile fp (show xs)

readSpaceStats :: FilePath -> IO (Map.Map Text [SpaceStats])
readSpaceStats fp = do
  t <- readFile fp
  let m = read t
  pure m

processResult :: (Integral a) => StatDType -> Map.Map Text [a] -> Maybe FilePath -> Maybe FilePath -> IO ()
processResult s m fp fpCheck = do
  let x = statD s <$> Map.mapKeys (:[]) m
  printOrg (expt (Just 3) <$> x)
  mapM_ (\fp' -> degradePrint 0.1 0.3 fp' x) fpCheck
  mapM_ (`writeResult` x) fp

main :: IO ()
main = do
  o <- execParser opts
  let !n = optionRuns o
  let !l = optionLength o
  let s = optionStatDType o
  let a = optionsExample o
  let r = optionRunType o
  let fp = fromMaybe ("other/" <> show r <> ".csv") (optionsGolden o)
  let record = bool Nothing (Just fp) (optionsRecord o)
  let check = bool Nothing (Just fp) (optionsRecordCheck o)

  case r of
    RunDefault-> do
      m <- execPerfT (times n) $ testExample (examplePattern a l)
      processResult s m record check

    RunExamples -> do
      m <- execPerfT (times n) $
         mapM_ testExample $
         (`examplePattern` l) <$>
         allExamples
      processResult s m record check

    RunExampleIO -> do
      m1 <- execPerfT time exampleIO
      printOrg (expt (Just 3) . fromIntegral <$> Map.mapKeys (:[]) m1)

      putStrLn "\nouter version\n"
      (_, (m', m2)) <- outer "total" time time exampleIO
      printOrg (expt (Just 3) . fromIntegral <$> Map.mapKeys (:[]) (m2 <> m'))

      putStrLn "\nslop version\n"
      (_, m3) <- slop "total" time exampleIO
      printOrg (expt (Just 3) . fromIntegral <$> Map.mapKeys (:[]) m3)

    RunSums-> do
      m <- statSums n l times
      printOrg (Map.mapKeys (:[]) $ Map.map (expt (Just 3) . statD s) m)

    RunNoOp -> do
      -- noop check
      noopPerf <- statNoOps (Just "other/noop.map") 10 n
      -- writeStats "other/noop.csv" id noopPerf
      printOrg noopPerf
    RunTicks -> do
      -- algo by tick style
      tickStylePerf <- statTicksSums n l s
      printOrg2DTranspose (fmap (expt (Just 3)) tickStylePerf)

    RunGauge -> do
      -- algo by tick style
      mapM_ testGaugeExample ((`examplePattern` l) <$> allExamples)
      testGauge "noop" (const ()) ()

    RunSpace -> do
      m <- execPerfT (toMeasureN n (space False)) $ testExample (examplePattern a l)
      printOrgSpace $ unlistify $ Map.mapKeys (:[]) m

    RunSpaceTime -> do
      let st = toMeasureN n ((,) <$> stepTime <*> space False)
      let pat = examplePattern a l
      m <- execPerfT st $ testExample pat
      printOrgSpaceTime $ unlistify $ Map.mapKeys (:[]) m

    RunTime -> do
      let st = toMeasureN n stepTime
      let pat = examplePattern a l
      m <- execPerfT st $ testExample pat
      printOrg (unlistify $ fmap (expt (Just 3) . fromIntegral) <$> Map.mapKeys (:[]) m)
