{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | basic measurement and callibration

module Main where

import Prelude hiding ((.))
import Options.Applicative
import Perf.Stats
import Perf.Algos
import Perf

data Options = Options
  { optionRuns :: Int,
    optionLength :: Int,
    optionStatType :: StatType
  } deriving (Eq, Show)

options :: Parser Options
options = Options <$>
  option auto (long "runs" <> short 'r' <> help "number of runs to perform") <*>
  option auto (long "length" <> short 'l' <> help "length of list") <*>
  parseStat

opts :: ParserInfo Options
opts = info (options <**> helper)
  (fullDesc <> progDesc "perf benchmarking" <> header "basic perf callibration")

main :: IO ()
main = do
  o <- execParser opts
  let !n = optionRuns o
  let !l = optionLength o
  let s = optionStatType o

  testBaseline n s

  mapM_ (\x -> testApps x n s) (allApps l)
