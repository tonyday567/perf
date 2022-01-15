{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | basic recursion

module Main where

import Prelude
import Perf.Cycle
import qualified Data.Text as T
import Data.Function hiding ((.))
import Options.Applicative
import Gauge
import Control.DeepSeq
import Perf.Algos
import Perf.Stats
import Data.Foldable

data RunType =
  RunGuage |
  RunInModule |
  RunAll deriving (Eq, Show)

data Options = Options
  { optionRun :: RunType,
    optionRuns :: Int,
    optionLength :: Int,
    optionStatType :: StatType
  } deriving (Eq, Show)

options :: Parser Options
options = Options <$>
  run <*>
  option auto (long "runs" <> short 'r' <> help "number of runs to perform") <*>
  option auto (long "length" <> short 'l' <> help "length of list") <*>
  parseStat

opts :: ParserInfo Options
opts = info (options <**> helper)
  (fullDesc <> progDesc "perf benchmarking" <> header "basic perf callibration")

run :: Parser RunType
run =
  flag' RunInModule (long "inmodule" <> help "just run in module tick") <|>
  flag' RunGuage (long "guage" <> help "run guage benchmark") <|>
  flag' RunAll (long "all" <> help "run all tests") <|>
  pure RunGuage

testGuage :: (NFData b) =>
  String -> (a -> b) -> a -> IO ()
testGuage label f a = do
  putStrLn label
  benchmarkWith defaultConfig (whnf f a)
  benchmarkWith defaultConfig (nf f a)

ticksInModule :: Int -> (a -> b) -> a -> IO ([Cycle], b)
ticksInModule n0 f a = go f a n0 []
  where
    go f' a' n ts
      | n <= 0 = pure (ts, f a)
      | otherwise = do
        (t, _) <- tickInModule f a
        go f' a' (n - 1) (t:ts)

tickInModule :: (a -> b) -> a -> IO (Cycle, b)
tickInModule !f !a = do
  !t <- rdtsc
  !a' <- pure (f a)
  !t' <- rdtsc
  pure (t' - t, a')
{-# INLINEABLE tickInModule #-}

runs :: (NFData b, NFData a) =>
  RunType -> T.Text -> (a -> b) -> a -> Int -> StatType -> IO ()
runs r label f a n s =
  case r of
    RunInModule ->
      ticksInModule n f a & reportStat ("inmodule " <> label <> " | ") s
    RunGuage -> do
      testGuage (T.unpack label) f a
      ticksInModule n f a & reportStat ("inmodule " <> label <> " | ") s
    RunAll -> do
      testGuage (T.unpack label) f a
      ticksInModule n f a & reportStat ("inmodule " <> label <> " | ") s
      testAllTicks label f a n s

main :: IO ()
main = do
  o <- execParser opts
  let !r = optionRun o
  let !n = optionRuns o
  let !l = optionLength o
  let s = optionStatType o
  let !ls = [1..l]
  _ <- warmup 100

  runs r "recurseTail" (recurseTail (+) 0) ls n s
  runs r "recurseCo" (recurseCo (+) 0) ls n s
  runs r "foldr" (foldr (+) 0) ls n s
  runs r "foldl'" (foldl' (+) 0) ls n s
  runs r "foldl" (foldl (+) 0) ls n s
  runs r "foldr'" (foldr' (+) 0) ls n s
