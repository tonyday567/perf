{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

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

data RunType = RunSums | RunGuage | RunNoOp | RunTicks | RunSpace | RunSpaceTime deriving (Eq, Show)

data Options = Options
  { optionRuns :: Int,
    optionLength :: Int,
    optionStatDType :: StatDType,
    optionRunType :: RunType,
    optionReportSum :: Text,
    optionsAlgoExample :: AlgoExample
  } deriving (Eq, Show)

parseRun :: Parser RunType
parseRun =
  flag' RunSums (long "sums" <> help "sums") <|>
  flag' RunNoOp (long "noop" <> help "no-ops") <|>
  flag' RunTicks (long "ticks" <> help "tick types") <|>
  flag' RunGuage (long "guage" <> help "guage comparison") <|>
  flag' RunSpace (long "space" <> help "space stats") <|>
  flag' RunSpaceTime (long "spacetime" <> help "space and time stats") <|>
  pure RunSums

options :: Parser Options
options = Options <$>
  option auto (value 1000 <> long "runs" <> short 'r' <> help "number of runs to perform") <*>
  option auto (value 1000 <> long "length" <> short 'l' <> help "length of list") <*>
  parseStatD <*>
  parseRun <*>
  strOption (value "polySum" <> long "sum" <> short 's' <> help "type of sum code") <*>
  parseAlgoExample

opts :: ParserInfo Options
opts = info (options <**> helper)
  (fullDesc <> progDesc "perf benchmarking" <> header "basic perf callibration")

-- * gauge experiment
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

main :: IO ()
main = do
  o <- execParser opts
  let !n = optionRuns o
  let !l = optionLength o
  let s = optionStatDType o
  let a = optionsAlgoExample o
  let r = optionRunType o
  let reportSum = optionReportSum o

  case r of
    RunSums-> do
      m <- runAllSums n l times
      writeFile "other/runsum.stats" (show m)
      printOrg (Map.mapKeys (:[]) $ Map.map (expt (Just 3) . statD s) m)

    RunNoOp -> do
      -- noop check
      noopPerf <- runNoOps (Just "other/noop.map") 10 n
      writeStats "other/noop.csv" id noopPerf
      printOrg noopPerf
    RunTicks -> do
      -- algo by tick style
      tickStylePerf <- testTickBySum n l s
      writeStats "other/basic.csv" id (fmap (expt (Just 3)) tickStylePerf)
      printOrg2DTranspose (fmap (expt (Just 3)) tickStylePerf)

    RunGuage -> do
      -- algo by tick style
      mapM_ testGaugeExample ((`examplePattern` l) <$> allAlgoExamples)
      testGauge "noop" (const ()) ()

    RunSpace -> do
      -- recordSpaceStats "other/space.stats" polySum [1..l] n
      -- putStrLn "Space stats recorded for polySum in other/space.stats"
      m <- execPerfT (toMeasureN n (space False)) $ testExample (examplePattern a l)
      printOrg (Map.map (prettyOrgSpace . foldl1 addSpace ) $ Map.mapKeys (:[]) m)

    RunSpaceTime -> do
      m <- runAllSums n l (\x -> toMeasureN x ((,) <$> stepTime <*> space2))
      writeFile "other/spacetime.stats" (show m)
      printOrgSpaceTime (m Map.! reportSum)
