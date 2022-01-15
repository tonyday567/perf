{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | basic measurement and callibration

module Main where

import Prelude hiding ((.))
import Perf.Cycle as Perf
import qualified Data.Text as T
import Data.Function
import Control.Category
import Control.Monad
import Options.Applicative
import Perf.Algos
import Perf.Stats
import Control.DeepSeq
import Data.Text (Text, unpack)

data Options = Options
  { optionRuns :: Int,
    optionLength :: Int,
    optionStatType :: StatType,
    optionAlgoType :: AlgoType
  } deriving (Eq, Show)

options :: Parser Options
options = Options <$>
  option auto (long "runs" <> short 'r' <> help "number of runs to perform") <*>
  option auto (long "length" <> short 'l' <> help "length of list") <*>
  parseStat <*>
  parseAlgo

opts :: ParserInfo Options
opts = info (options <**> helper)
  (fullDesc <> progDesc "perf benchmarking" <> header "basic perf callibration")

tickInModule :: (a -> b) -> a -> IO (Cycle, b)
tickInModule !f !a = do
  !t <- rdtsc
  !a' <- pure (f a)
  !t' <- rdtsc
  pure (t' - t, a')
{-# INLINE tickInModule #-}

ticksRec :: ((a -> b) -> a -> IO (Cycle, b)) -> Int -> (a -> b) -> a -> IO ([Cycle], b)
ticksRec tickf n0 f a = go f a n0 []
  where
    go f' a' n ts
      | n <= 0 = pure (ts, f a)
      | otherwise = do
        (t, _) <- tickf f a
        go f' a' (n - 1) (t:ts)

ticksR :: ((a -> b) -> a -> IO (Cycle, b)) -> Int -> (a -> b) -> a -> IO ([Cycle], b)
ticksR tickf n0 f a = fmap (\xs -> (fmap fst xs, snd (head xs))) (replicateM n0 (tickf f a))

ticksInModule :: Int -> (a -> b) -> a -> IO ([Cycle], b)
ticksInModule n0 f a = go f a n0 []
  where
    go f' a' n ts
      | n <= 0 = pure (ts, f a)
      | otherwise = do
        (t, _) <- tickInModule f a
        go f' a' (n - 1) (t:ts)

runs :: (NFData a, NFData b) =>
  Text -> (a -> b) -> a -> Int -> StatType -> IO ()
runs label tf ta n s = do
  putStrLn $ unpack label
  replicateM n (Perf.tick tf ta) & fmap (fmap fst >>> stat s >>> T.unpack >>> ("replicateM Perf.tick "<>)) & (>>= putStrLn)
  replicateM n (tickInModule tf ta) & fmap (fmap fst >>> stat s >>> T.unpack >>> ("replicateM tickInModule "<>)) & (>>= putStrLn)
  Perf.ticks n tf ta & fmap (fst >>> stat s >>> T.unpack >>> ("Perf.ticks " <>)) & (>>= putStrLn)
  Perf.multi Perf.tick n tf ta & fmap (fst >>> stat s >>> T.unpack >>> ("Perf.multi Perf.tick " <>)) & (>>= putStrLn)
  Perf.multi Perf.tickForce n tf ta & fmap (fst >>> stat s >>> T.unpack >>> ("Perf.multi Perf.tickForce " <>)) & (>>= putStrLn)
  Perf.multi tickInModule n tf ta & fmap (fst >>> stat s >>> T.unpack >>> ("Perf.multi tickInModule " <>)) & (>>= putStrLn)
  ticksInModule n tf ta & fmap (fst >>> stat s >>> T.unpack >>> ("ticksInModule " <>)) & (>>= putStrLn)
  ticksR tickInModule n tf ta & fmap (fst >>> stat s >>> T.unpack >>> ("ticksR tickInModule " <>)) & (>>= putStrLn)
  ticksRec tickInModule n tf ta & fmap (fst >>> stat s >>> T.unpack >>> ("ticksRec tickInModule " <>)) & (>>= putStrLn)
  ticksR tick n tf ta & fmap (fst >>> stat s >>> T.unpack >>> ("ticksR tick " <>)) & (>>= putStrLn)
  ticksRec tick n tf ta & fmap (fst >>> stat s >>> T.unpack >>> ("ticksRec tick " <>)) & (>>= putStrLn)

main :: IO ()
main = do
  o <- execParser opts
  let !n = optionRuns o
  let !l = optionLength o
  let !ls = [1..l]
  let !s = optionStatType o
  let !a = optionAlgoType o
  print a
  _ <- warmup 100

  case a of
    AlgoFuseSum -> runs "fuseSum" fuseSum l n s
    AlgoFuseConst -> runs "fuseConst" fuseConst l n s
    AlgoRecSum -> runs "recSum" recSum ls n s
    AlgoMonoSum -> runs "monoSum" monoSum ls n s
    AlgoPolySum -> runs "polySum" polySum ls n s
    AlgoLambdaSum -> runs "lambdaSum" lambdaSum ls n s
    AlgoMapInc -> runs "MapInc" mapInc ls n s
