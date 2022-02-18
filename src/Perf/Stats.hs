{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- |

module Perf.Stats
  ( averageD,
    medianD,
    tenthD,
    averageSecsD,
    StatDType (..),
    statD,
    parseStatD,

    -- stat reporting
    addStat,
    readStats,
    writeStats,

  ) where

import Data.Text (Text)
import NumHask.Space (quantile)
import Options.Applicative
import qualified Data.Map.Strict as Map
import Control.Monad.State.Lazy
import Box.Csv
import Box

-- convert a list (of Integrals) to a double stat
medianD :: (Integral a) => [a] -> Double
medianD = quantile 0.5 . fmap Prelude.fromIntegral

averageD :: (Integral a) => [a] -> Double
averageD xs = (fromIntegral . Prelude.toInteger . sum $ xs) / (fromIntegral . length $ xs)

tenthD :: (Integral a) => [a] -> Double
tenthD = quantile 0.1 . fmap Prelude.fromIntegral

averageSecsD :: (Integral a) => [a] -> Double
averageSecsD xs = (fromIntegral . Prelude.toInteger . sum $ xs) / (fromIntegral . length $ xs) / 2.5e9

data StatDType = StatAverage | StatMedian | StatBest | StatSecs deriving (Eq, Show)

statD :: StatDType -> (Integral a) => [a] -> Double
statD StatBest = tenthD
statD StatMedian = medianD
statD StatAverage = averageD
statD StatSecs = averageSecsD

parseStatD :: Parser StatDType
parseStatD =
  flag' StatBest (long "best" <> help "report upper decile") <|>
  flag' StatMedian (long "median" <> help "report median") <|>
  flag' StatAverage (long "average" <> help "report average") <|>
  flag' StatSecs (long "averagesecs" <> help "report average in seconds") <|>
  pure StatAverage

-- stat reporting
addStat :: (Ord k, Monad m) => k -> s -> StateT (Map.Map k s) m ()
addStat label s = do
  modify (Map.insert label s)

csvFile :: CsvConfig
csvFile = CsvConfig "./other/default.csv" ',' NoHeader

writeStats :: FilePath -> (a -> Text) -> Map.Map [Text] a -> IO ()
writeStats fp f m =
  glue <$> rowCommitter (csvFile { file = fp }) (\(l,v) -> l <> [f v]) <*|> qList (Map.toList m)

readStats :: FilePath -> IO (Map.Map [Text] Text)
readStats fp = do
  r <- runCsv (csvFile { file = fp  }) fields
  pure $ Map.fromList [(init x, last x) | (Right x) <- r]

