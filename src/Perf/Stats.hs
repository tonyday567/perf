{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- |

module Perf.Stats
  ( average,
    median,
    tenth,
    averageSecs,
    StatType (..),
    stat,
    statD,
    parseStat,
    reportStat,
    testAllTicks,
    testApps,
    testBaseline,
    testBaselineP,
  ) where

import Perf
import qualified Data.Text as T
import Data.Text (Text)
import Data.FormatN
import NumHask.Space (quantile)
import Perf.Algos
import Control.DeepSeq
import Options.Applicative
import Data.Function
import Control.Category hiding ((.))
import Control.Monad
import Data.Semigroup
import qualified Data.Map.Strict as Map

median :: [Cycle] -> Text
median = comma (Just 3) . quantile 0.5 . fmap Prelude.fromIntegral

medianD :: [Cycle] -> Double
medianD = quantile 0.5 . fmap Prelude.fromIntegral

average :: [Cycle] -> Text
average = comma (Just 3) . (\xs -> (fromIntegral . Prelude.toInteger . sum $ xs) / (fromIntegral . length $ xs))

averageD :: [Cycle] -> Double
averageD xs = (fromIntegral . Prelude.toInteger . sum $ xs) / (fromIntegral . length $ xs)

tenth :: [Cycle] -> Text
tenth = comma (Just 3) . quantile 0.1 . fmap Prelude.fromIntegral

tenthD :: [Cycle] -> Double
tenthD = quantile 0.1 . fmap Prelude.fromIntegral

data StatType = StatAverage | StatMedian | StatBest | StatSecs deriving (Eq, Show)

averageSecs :: [Cycle] -> Text
averageSecs = expt (Just 3) . (\xs -> (fromIntegral . Prelude.toInteger . sum $ xs) / (fromIntegral . length $ xs) / 2.5e9)

averageSecsD :: [Cycle] -> Double
averageSecsD xs = (fromIntegral . Prelude.toInteger . sum $ xs) / (fromIntegral . length $ xs) / 2.5e9

stat :: StatType -> [Cycle] -> Text
stat StatBest = tenth
stat StatMedian = median
stat StatAverage = average
stat StatSecs = averageSecs

statD :: StatType -> [Cycle] -> Double
statD StatBest = tenthD
statD StatMedian = medianD
statD StatAverage = averageD
statD StatSecs = averageSecsD


parseStat :: Parser StatType
parseStat =
  flag' StatBest (long "best" <> help "report upper decile") <|>
  flag' StatMedian (long "median" <> help "report median") <|>
  flag' StatAverage (long "average" <> help "report average") <|>
  flag' StatSecs (long "averagesecs" <> help "report average in seconds") <|>
  pure StatAverage

reportStat :: Text -> StatType -> IO ([Cycle], b) -> IO ()
reportStat label s x = x & fmap (fst >>> stat s >>> (label<>) >>> T.unpack) & (>>= putStrLn)

testAllTicks :: (NFData a, NFData b) =>
  Text -> (a -> b) -> a -> Int -> StatType -> IO ()
testAllTicks label f a n s = do
  ticks n f a & reportStat ("ticks " <> label <> " | ") s
  multi tick n f a & reportStat ("multi tick " <> label <> " | ") s
  multi tickWHNF n f a & reportStat ("multi tickWHNF " <> label <> " | ") s
  multi tickLazy n f a & reportStat ("multi tickLazy " <> label <> " | ") s
  multi tickForce n f a & reportStat ("multi tickForce " <> label <> " | ") s
  multi tickForceArgs n f a & reportStat ("multi tickForceArgs " <> label <> " | ") s

testApps :: AlgoApplication Int -> Int -> StatType -> IO ()
testApps (ApplicationFuseSum label f a) n s = testAllTicks label f a n s
testApps (ApplicationFuseConst label f a) n s = testAllTicks label f a n s
testApps (ApplicationRecSum label f a) n s = testAllTicks label f a n s
testApps (ApplicationMonoSum label f a) n s = testAllTicks label f a n s
testApps (ApplicationPolySum label f a) n s = testAllTicks label f a n s
testApps (ApplicationLambdaSum label f a) n s = testAllTicks label f a n s
testApps (ApplicationMapInc label f a) n s = testAllTicks label f a n s

testBaseline :: Int -> StatType -> IO ()
testBaseline n s = do
  replicateM 10 tick_ & fmap (show >>> ("tick_: "<>)) & (>>= putStrLn)
  replicateM n tick_ & fmap (stat s >>> T.unpack >>> ("tick_: "<>)) & (>>= putStrLn)
  replicateM 10 (tick (const ()) ()) & fmap (fmap fst >>> show >>> ("const (): "<>)) & (>>= putStrLn)
  multi tick n (const ()) () & reportStat "const ()|" s
  replicateM n (tickIO (pure ())) & fmap (fmap fst >>> stat s >>> T.unpack >>> ("tickIO (pure ()): "<>)) & (>>= putStrLn)

-- testBaseline' :: Int -> StatType -> IO ()
testBaselineP :: p1 -> p2 -> IO ((), Map.Map Text (Sum Cycle))
testBaselineP _ _ = runPerfT (Sum <$> cycle') $ do
  fap "const" (const ()) ()
  fam "pure" (pure ())
