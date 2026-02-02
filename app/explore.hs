{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | basic measurement and callibration
module Main where

import Data.List (intercalate)
import GHC.Generics
import Optics.Core
import Options.Applicative
import Options.Applicative.Help.Pretty
import Perf
import Prelude

data Run = RunExample | RunSums | RunLengths deriving (Eq, Show)

data AppConfig = AppConfig
  { appRun :: Run,
    appExample :: Example,
    appReportOptions :: ReportOptions
  }
  deriving (Eq, Show, Generic)

defaultAppConfig :: AppConfig
defaultAppConfig = AppConfig RunExample ExampleSum defaultReportOptions

parseRun :: Parser Run
parseRun =
  flag' RunSums (long "sums" <> help "run on sum algorithms")
    <|> flag' RunLengths (long "lengths" <> help "run on length algorithms")
    <|> flag' RunExample (long "example" <> help "run on the example algorithm" <> style (annotate bold))
    <|> pure RunExample

appParser :: AppConfig -> Parser AppConfig
appParser def =
  AppConfig
    <$> parseRun
    <*> parseExample
    <*> parseReportOptions (view #appReportOptions def)

appConfig :: AppConfig -> ParserInfo AppConfig
appConfig def =
  info
    (appParser def <**> helper)
    (fullDesc <> header "Examples of perf usage (defaults in bold)")

main :: IO ()
main = do
  o <- execParser (appConfig defaultAppConfig)
  let repOptions = appReportOptions o
  let n = reportN repOptions
  let s = reportStatDType repOptions
  let mt = reportMeasureType repOptions
  let c = reportClock repOptions
  let !l = reportLength repOptions
  let a = appExample o
  let r = appRun o

  case r of
    RunExample -> do
      reportMain
        repOptions
        (intercalate "-" [show r, show a, show l])
        (testExample . examplePattern a)
    RunSums -> do
      m <- statSums n l (measureDs mt c)
      let o' = replaceDefaultFilePath (intercalate "-" [show r, show mt, show n, show l, show s]) repOptions
      report o' (statify s m)
    RunLengths -> do
      m <- statLengths n l (measureDs mt c)
      let o' = replaceDefaultFilePath (intercalate "-" [show r, show mt, show n, show l, show s]) repOptions
      report o' (statify s m)
