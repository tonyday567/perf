{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | basic measurement and callibration
module Main where

import Data.List (intercalate, nub)
import GHC.Generics
import Optics.Core
import Options.Applicative
import Options.Applicative.Help.Pretty
import Perf
import Prelude
import Control.Monad

data Run = RunSum | RunNub deriving (Eq, Show)

data AppConfig = AppConfig
  { appRun :: Run,
    appReportOptions :: ReportOptions
  }
  deriving (Eq, Show, Generic)

defaultAppConfig :: AppConfig
defaultAppConfig = AppConfig RunSum defaultReportOptions

parseRun :: Parser Run
parseRun =
  flag' RunSum (long "sum" <> help "measure sum performance")
  <|> flag' RunNub (long "nub" <> help "measure nub performance")
    <|> pure RunNub

appParser :: AppConfig -> Parser AppConfig
appParser def =
  AppConfig
    <$> parseRun
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
  let !l = reportLength repOptions
  let r = appRun o

  case r of
    RunSum -> do reportMain repOptions (intercalate "-" ["sum", show r, show l]) (\l' ->  void $ ffap "sum" sum [1..l'::Int])
    RunNub -> do reportMain repOptions (intercalate "-" ["nub", show r, show l]) (\l' ->  void $ ffap "nub" nub [1..l'::Int])
