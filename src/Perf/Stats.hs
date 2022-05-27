{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

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
    ordy,
    allStats,
    statify,
  )
where

import Control.Monad.State.Lazy
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack)
import NumHask.Space (quantile)
import Options.Applicative

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
  flag' StatBest (long "best" <> help "report upper decile")
    <|> flag' StatMedian (long "median" <> help "report median")
    <|> flag' StatAverage (long "average" <> help "report average")
    <|> flag' StatSecs (long "averagesecs" <> help "report average in seconds")
    <|> pure StatAverage

-- stat reporting
addStat :: (Ord k, Monad m) => k -> s -> StateT (Map.Map k s) m ()
addStat label s = do
  modify (Map.insert label s)

ordy :: Int -> [Text]
ordy f = zipWith (\x s -> (pack . show) x <> s) [1 .. f] (["st", "nd", "rd"] <> repeat "th")

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

statify :: (Ord a) => StatDType -> Map.Map a [[Double]] -> Map.Map [a] [Double]
statify s m = fmap (statD s) . List.transpose <$> Map.mapKeys (: []) m
