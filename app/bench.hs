{-# LANGUAGE OverloadedStrings #-}

-- | basic measurement and callibration
module Main where

import Control.Monad
import Data.List (intercalate)
import Options.Applicative
import Perf
import System.Exit
import Prelude

data Options = Options
  { optionN :: Int,
    optionLength :: Int,
    optionStatDType :: StatDType,
    optionMeasureType :: MeasureType,
    optionExample :: Example,
    optionGolden :: Golden,
    optionReportConfig :: ReportConfig,
    optionRawStats :: Bool
  }
  deriving (Eq, Show)

options :: Parser Options
options =
  Options
    <$> option auto (value 1000 <> long "runs" <> short 'n' <> help "number of runs to perform")
    <*> option auto (value 1000 <> long "length" <> short 'l' <> help "length of list")
    <*> parseStatD
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

main :: IO ()
main = do
  o <- execParser opts
  let !n = optionN o
  let !l = optionLength o
  let s = optionStatDType o
  let a = optionExample o
  let mt = optionMeasureType o
  let gold = goldenFromOptions [show r, show n, show l, show mt, show s] (optionGolden o)
  let w = optionRawStats o
  let raw =
        "other/"
          <> intercalate "-" [show r, show n, show l, show mt]
          <> ".map"
  let cfg = optionReportConfig o

  m <- execPerfT (measureDs mt n) $ testExample (examplePattern a l)
  when w (writeFile raw (show m))
  code <- report cfg gold (measureLabels mt) (statify s m)
  exitWith code
