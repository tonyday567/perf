{-# LANGUAGE OverloadedStrings #-}

-- | Statistical choices for multiple performance measurements.
module Perf.Stats
  ( average,
    median,
    tenth,
    averageI,
    StatDType (..),
    statD,
    statDs,
    parseStatD,
    -- stat reporting
    addStat,
    ordy,
    allStats,
    statify,
  )
where

import Control.Monad.State.Lazy
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text (Text, pack)
import NumHask.Space (quantile)
import Options.Applicative

-- | Compute the median
median :: [Double] -> Double
median = quantile 0.5

-- | Compute the average
average :: [Double] -> Double
average xs = sum xs / (fromIntegral . length $ xs)

-- | Compute the tenth percentile
tenth :: [Double] -> Double
tenth = quantile 0.1

-- | Compute the average of an Integral
averageI :: (Integral a) => [a] -> Double
averageI xs = sum (fromIntegral <$> xs) / (fromIntegral . length $ xs)

-- | Command-line options for type of statistic.
data StatDType = StatAverage | StatMedian | StatBest deriving (Eq, Show)

-- | Compute a statistic.
statD :: StatDType -> [Double] -> Double
statD StatBest = tenth
statD StatMedian = median
statD StatAverage = average

-- | Compute a list of statistics.
statDs :: StatDType -> [[Double]] -> [Double]
statDs StatBest = fmap tenth . List.transpose
statDs StatMedian = fmap median . List.transpose
statDs StatAverage = fmap average . List.transpose

-- | Parse command-line 'StatDType' options.
parseStatD :: Parser StatDType
parseStatD =
  flag' StatBest (long "best" <> help "report upper decile")
    <|> flag' StatMedian (long "median" <> help "report median")
    <|> flag' StatAverage (long "average" <> help "report average")
    <|> pure StatAverage

-- | Add a statistic to a State Map
addStat :: (Ord k, Monad m) => k -> s -> StateT (Map.Map k s) m ()
addStat label s = do
  modify (Map.insert label s)

-- | Linguistic conversion of an ordinal
ordy :: Int -> [Text]
ordy f = zipWith (\x s -> (pack . show) x <> s) [1 .. f] (["st", "nd", "rd"] <> repeat "th")

-- | Compute all stats.
allStats :: Int -> Map.Map [Text] [[Double]] -> Map.Map [Text] [Double]
allStats f m =
  Map.fromList $
    mconcat
      [ mconcat ((\(ks, xss) -> zipWith (\l xs -> (ks <> [l], xs)) (ordy f) xss) <$> mlist),
        (\(ks, xss) -> (ks <> ["best"], quantile 0.1 <$> List.transpose xss)) <$> mlist,
        (\(ks, xss) -> (ks <> ["median"], quantile 0.5 <$> List.transpose xss)) <$> mlist,
        (\(ks, xss) -> (ks <> ["average"], av <$> List.transpose xss)) <$> mlist
      ]
  where
    mlist = Map.toList m
    av xs = sum xs / (fromIntegral . length $ xs)

-- | Convert a Map of performance result to a statistic.
statify :: (Ord a) => StatDType -> Map.Map a [[Double]] -> Map.Map [a] [Double]
statify s m = fmap (statD s) . List.transpose <$> Map.mapKeys (: []) m
