{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | basic measurement and callibration

module Main where

import Prelude hiding (cycle)
import Options.Applicative
import Perf.Stats
import Gauge
import Perf.Algos
import qualified Data.Text.IO as Text
import Data.Text (Text)
import Perf.Tick
import Control.DeepSeq
import Control.Monad.State.Lazy
import qualified Data.Map.Strict as Map
import System.CPUTime.Rdtsc
import Perf
import GHC.Stats

data RunType = RunGuage | RunNoOp | RunTickTypes | RunTickInModule | RunSpace | RunSpaceTime deriving (Eq, Show)

data Options = Options
  { optionRuns :: Int,
    optionLength :: Int,
    optionStatType :: StatType,
    optionRunType :: RunType,
    optionReportSum :: Text
  } deriving (Eq, Show)

parseRun :: Parser RunType
parseRun =
  flag' RunNoOp (long "noop" <> help "no-ops") <|>
  flag' RunTickTypes (long "ticktypes" <> help "tick types") <|>
  flag' RunGuage (long "guage" <> help "guage comparison") <|>
  flag' RunTickInModule (long "inmodule" <> help "tick in module") <|>
  flag' RunSpace (long "space" <> help "space stats") <|>
  flag' RunSpaceTime (long "spacetime" <> help "space and time stats") <|>
  pure RunNoOp

options :: Parser Options
options = Options <$>
  option auto (long "runs" <> short 'r' <> help "number of runs to perform") <*>
  option auto (long "length" <> short 'l' <> help "length of list") <*>
  parseStat <*>
  parseRun <*>
  strOption (long "sum" <> short 's' <> help "type of sum code")

opts :: ParserInfo Options
opts = info (options <**> helper)
  (fullDesc <> progDesc "perf benchmarking" <> header "basic perf callibration")

-- * gauge experiment
testGuage :: (NFData b) =>
  Text -> (a -> b) -> a -> IO ()
testGuage label f a = do
  Text.putStrLn label
  benchmarkWith defaultConfig (whnf f a)
  benchmarkWith defaultConfig (nf f a)

runGuage :: SumAlgo Int -> IO ()
runGuage (SumFuse l f a) = testGuage l f a
runGuage (SumPoly l f a) = testGuage l f a
runGuage (SumMono l f a) = testGuage l f a
runGuage (SumLambda l f a) = testGuage l f a


-- * inplace ticks
ticksInModule :: Int -> (a -> b) -> a -> IO ([Word64], b)
ticksInModule n0 f a = go f a n0 []
  where
    go f' a' n ts
      | n <= 0 = pure (ts, f a)
      | otherwise = do
        (t, _) <- tickInModule f a
        go f' a' (n - 1) (t:ts)

tickInModule :: (a -> b) -> a -> IO (Word64, b)
tickInModule !f !a = do
  !t <- rdtsc
  !a' <- pure $! f a
  !t' <- rdtsc
  pure (t' - t, a')
{-# INLINEABLE tickInModule #-}

testTickInModule :: Text -> (t -> b) -> t -> Int -> StatType -> StateT (Map.Map [Text] Text) IO ()
testTickInModule l f a n s = do
  addStat [l,"ticksInModule"] . stat s =<< lift (fst <$> ticksInModule n f a)

testSumInModule :: SumAlgo Int -> Int -> StatType -> StateT (Map.Map [Text] Text) IO ()
testSumInModule (SumFuse label f a) n s = testTickInModule label f a n s
testSumInModule (SumMono label f a) n s = testTickInModule label f a n s
testSumInModule (SumPoly label f a) n s = testTickInModule label f a n s
testSumInModule (SumLambda label f a) n s = testTickInModule label f a n s


recordSpaceStats :: FilePath -> (a -> b) -> a -> Int -> IO ()
recordSpaceStats fp f a n = do
  xs <- execPerfT (toMeasureN n (space False)) (f |$| a)
  writeFile fp (show xs)

readSpaceStats :: FilePath -> IO (Map.Map Text [SpaceStats])
readSpaceStats fp = do
  t <- readFile fp
  let m = read t
  pure m

-- * ghcStats usage

recordGHCStats :: FilePath -> (a -> b) -> a -> Int -> IO ()
recordGHCStats fp f a n = do
  xs <- execPerfT (toMeasureN n ghcStats) (f |$| a)
  writeFile fp (show xs)

readGHCStats :: FilePath -> IO (Map.Map Text [Maybe (RTSStats, RTSStats)])
readGHCStats fp = do
  t <- readFile fp
  let m = read t
  pure m

diffRTSStats :: RTSStats -> RTSStats -> RTSStats
diffRTSStats (RTSStats x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 (GCDetails g1 g2 g3 g4 g5 g6 g7 g8 g9 g10 g11 g12 g13 g14 g15 g16)) (RTSStats x1' x2' x3' x4' x5' x6' x7' x8' x9' x10' x11' x12' x13' x14' x15' x16' x17' x18' x19' x20' x21' x22' x23' x24' x25' x26' x27' (GCDetails g1' g2' g3' g4' g5' g6' g7' g8' g9' g10' g11' g12' g13' g14' g15' g16')) = RTSStats (x1'-x1) (x2'-x2) (x3'-x3) (x4'-x4) (x5'-x5) (x6'-x6) (x7'-x7) (x8'-x8) (x9'-x9) (x10'-x10) (x11'-x11) (x12'-x12) (x13'-x13) (x14'-x14) (x15'-x15) (x16'-x16) (x17'-x17) (x18'-x18) (x19'-x19) (x20'-x20) (x21'-x21) (x22'-x22) (x23'-x23) (x24'-x24) (x25'-x25) (x26'-x26) (x27'-x27) (GCDetails (g1'-g1) (g2'-g2) (g3'-g3) (g4'-g4) (g5'-g5) (g6'-g6) (g7'-g7) (g8'-g8) (g9'-g9) (g10'-g10) (g11'-g11) (g12'-g12) (g13'-g13) (g14'-g14) (g15'-g15) (g16'-g16))


-- RTSStats {gcs = 0, major_gcs = 0, allocated_bytes = 0, max_live_bytes = 0, max_large_objects_bytes = 0, max_compact_bytes = 0, max_slop_bytes = 0, max_mem_in_use_bytes = 0, cumulative_live_bytes = 0, copied_bytes = 0, par_copied_bytes = 0, cumulative_par_max_copied_bytes = 0, cumulative_par_balanced_copied_bytes = 0, init_cpu_ns = 325000, init_elapsed_ns = 4215545, mutator_cpu_ns = 332000, mutator_elapsed_ns = 608629, gc_cpu_ns = 0, gc_elapsed_ns = 0, cpu_ns = 332000, elapsed_ns = 608629, nonmoving_gc_sync_cpu_ns = 0, nonmoving_gc_sync_elapsed_ns = 0, nonmoving_gc_sync_max_elapsed_ns = 0, nonmoving_gc_cpu_ns = 0, nonmoving_gc_elapsed_ns = 0, nonmoving_gc_max_elapsed_ns = 0, gc = GCDetails {gcdetails_gen = 0, gcdetails_threads = 0, gcdetails_allocated_bytes = 0, gcdetails_live_bytes = 0, gcdetails_large_objects_bytes = 0, gcdetails_compact_bytes = 0, gcdetails_slop_bytes = 0, gcdetails_mem_in_use_bytes = 0, gcdetails_copied_bytes = 0, gcdetails_par_max_copied_bytes = 0, gcdetails_par_balanced_copied_bytes = 0, gcdetails_sync_elapsed_ns = 0, gcdetails_cpu_ns = 0, gcdetails_elapsed_ns = 0, gcdetails_nonmoving_gc_sync_cpu_ns = 0, gcdetails_nonmoving_gc_sync_elapsed_ns = 0}}


main :: IO ()
main = do
  o <- execParser opts
  let !n = optionRuns o
  let !l = optionLength o
  let s = optionStatType o
  let r = optionRunType o
  let reportSum = optionReportSum o

  case r of
    RunNoOp -> do
      -- noop check
      noopPerf <- testNoOps (Just "other/noop.map") 10 n
      writeStats "other/noop.csv" id noopPerf
      printOrg noopPerf
    RunTickTypes -> do
      -- algo by tick style
      tickStylePerf <- testStyleBySum n l s
      writeStats "other/basic.csv" id tickStylePerf
      printOrg2DTranspose tickStylePerf
    RunGuage -> do
      -- algo by tick style
      mapM_ runGuage (allSums l)
      testGuage "noop" (const ()) ()
    RunTickInModule -> do
      -- algo by tick style
      perfModule <- flip execStateT Map.empty $ mapM_ (\x -> testSumInModule x n s) (allSums l)
      print perfModule

    RunSpace -> do
      recordSpaceStats "other/space.stats" polySum [1..l] n
      putStrLn "Space stats recorded for polySum in other/space.stats"
    RunSpaceTime -> do
      m <- testSums n l ((,) <$> cycle <*> space2)
      writeFile "other/spacetime.stats" (show m)
      printOrgSpaceTime (m Map.! reportSum)
