{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

import Test.Tasty.Bench
import Test.Tasty
import Perf
import Options.Applicative
import GHC.Generics
import Control.Monad
import Data.List (intercalate)
import Optics.Core

data AppConfig = AppConfig
  { appReportOptions :: ReportOptions,
    appFib :: Int
  }
  deriving (Eq, Show, Generic)

defaultAppConfig :: AppConfig
defaultAppConfig = AppConfig defaultReportOptions 20

appConfig :: AppConfig -> ParserInfo AppConfig
appConfig def =
  info
    (appParser def <**> helper)
    (fullDesc <> progDesc "tasty comparison" <> header "comparing tasty and perf")

appParser :: AppConfig -> Parser AppConfig
appParser def =
  AppConfig
    <$> parseReportOptions (view #appReportOptions def)
    <*> option auto (value (view #appFib def) <> long "fib" <> help "fib n")

fibo :: Int -> Integer
fibo n = if n < 2 then toInteger n else fibo (n - 1) + fibo (n - 2)

main :: IO ()
main = do
  o <- execParser (appConfig defaultAppConfig)
  let repOptions = appReportOptions o
  let n = appFib o
  reportMain repOptions (intercalate "-" ["fib",show (reportN repOptions), show (reportMeasureType repOptions)]) (void $ fap "fib" fibo n)
  print =<< measureCpuTime (mkTimeout 1000000) (RelStDev 0.05) (nf fibo 5)

{-
  defaultMain
   [ bgroup "Fibonacci numbers"
      [ bench "fifth"     $ nf fibo  5
      , bench "tenth"     $ nf fibo 10
      , bench "twentieth" $ nf fibo 20
      ]
    ]
-}
