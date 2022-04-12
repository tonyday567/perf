{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- |

module Perf.Stats
  ( average,
    median,
    tenth,
    averageI,
    averageSecs,
    StatDType (..),
    statD,
    statDs,
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
import qualified Data.List as List

median :: [Double] -> Double
median = quantile 0.5

average :: [Double] -> Double
average xs = sum xs / (fromIntegral . length $ xs)

tenth :: [Double] -> Double
tenth = quantile 0.1

averageI :: (Integral a) => [a] -> Double
averageI xs = sum (fromIntegral <$> xs) / (fromIntegral . length $ xs)

averageSecs :: [Double] -> Double
averageSecs xs = sum xs / (fromIntegral . length $ xs) / 2.5e9

data StatDType = StatAverage | StatMedian | StatBest | StatSecs deriving (Eq, Show)

statD :: StatDType -> [Double] -> Double
statD StatBest = tenth
statD StatMedian = median
statD StatAverage = average
statD StatSecs = averageSecs

statDs :: StatDType -> [[Double]] -> [Double]
statDs StatBest = fmap tenth . List.transpose
statDs StatMedian = fmap median . List.transpose
statDs StatAverage = fmap average . List.transpose
statDs StatSecs = fmap averageSecs . List.transpose

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

