{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | basic measurement and callibration

module Main where

import Prelude hiding ((.))
import Perf.Tick
import qualified Data.Text as T
import Data.Function
import Control.Category
import Control.Monad
import Options.Applicative
import Control.DeepSeq
import Perf.Algos
import Perf.Stats
import Data.Text (Text, unpack)

data Options = Options
  { optionRuns :: Int,
    optionLength :: Int,
    optionAlgoType :: AlgoType,
    optionStatType :: StatType
  } deriving (Eq, Show)

options :: Parser Options
options = Options <$>
  option auto (long "runs" <> short 'r' <> help "number of runs to perform") <*>
  option auto (long "length" <> short 'l' <> help "length of list") <*>
  parseAlgo <*>
  parseStat

opts :: ParserInfo Options
opts = info (options <**> helper)
  (fullDesc <> progDesc "perf benchmarking" <> header "basic perf callibration")

-- * in-module variations on a tick counter

tickNoPragma :: (a -> b) -> a -> IO (Word64, b)
tickNoPragma !f !a = do
  !t <- rdtsc
  !a' <- pure $! f a
  !t' <- rdtsc
  pure (t' - t, a')

-- | tick where the arguments are lazy, so measurement may include evaluation of thunks that may constitute f and/or a
tickLazyS :: (a -> b) -> a -> IO (Word64, b)
tickLazyS f a = do
  !t <- rdtsc
  !a' <- pure (f a)
  !t' <- rdtsc
  pure (t' - t, a')
{-# INLINE tickLazyS #-}

tickInline :: (a -> b) -> a -> IO (Word64, b)
tickInline !f !a = do
  !t <- rdtsc
  !a' <- pure (f a)
  !t' <- rdtsc
  pure (t' - t, a')
{-# INLINE tickInline #-}

tickInlineable :: (a -> b) -> a -> IO (Word64, b)
tickInlineable !f !a = do
  !t <- rdtsc
  !a' <- pure (f a)
  !t' <- rdtsc
  pure (t' - t, a')
{-# INLINEABLE tickInlineable #-}

-- | tick where the arguments are lazy, so measurement may include evaluation of thunks that may constitute f and/or a
tickNoinline :: (a -> b) -> a -> IO (Word64, b)
tickNoinline !f !a = do
  !t <- rdtsc
  !a' <- pure (f a)
  !t' <- rdtsc
  pure (t' - t, a')
{-# NOINLINE tickNoinline #-}

tickInline1 :: (a -> b) -> a -> IO (Word64, b)
tickInline1 !f !a = do
  !t <- rdtsc
  !a' <- pure (f a)
  !t' <- rdtsc
  pure (t' - t, a')
{-# INLINE [1] tickInline1 #-}

tickInline2 :: (a -> b) -> a -> IO (Word64, b)
tickInline2 !f !a = do
  !t <- rdtsc
  !a' <- pure (f a)
  !t' <- rdtsc
  pure (t' - t, a')
{-# INLINE [2] tickInline2 #-}

tickInline1' :: (a -> b) -> a -> IO (Word64, b)
tickInline1' !f !a = do
  !t <- rdtsc
  !a' <- pure (f a)
  !t' <- rdtsc
  pure (t' - t, a')
{-# INLINE [~1] tickInline1' #-}

tickInline2' :: (a -> b) -> a -> IO (Word64, b)
tickInline2' !f !a = do
  !t <- rdtsc
  !a' <- pure (f a)
  !t' <- rdtsc
  pure (t' - t, a')
{-# INLINE [~2] tickInline2' #-}

multi' :: ((a -> b) -> a -> IO (Word64, b)) -> Int -> (a -> b) -> a -> IO ([Word64], b)
multi' tickf n0 f a = fmap (\xs -> (fmap fst xs, snd (head xs))) (replicateM n0 (tickf f a))
{-# INLINEABLE multi' #-}

ticks' :: (NFData a, NFData b) => Int -> (a -> b) -> a -> IO ([Word64], b)
ticks' n0 f a = fmap (\xs -> (fmap fst xs, snd (head xs))) (replicateM n0 (tickForce f a))
{-# INLINEABLE ticks' #-}

ticksNoPragma :: Int -> (a -> b) -> a -> IO ([Word64], b)
ticksNoPragma n0 f a = go f a n0 []
  where
    go f' a' n ts
      | n <= 0 = pure (ts, f a)
      | otherwise = do
        (t, _) <- tickNoPragma f a
        go f' a' (n - 1) (t:ts)

ticksLazy :: Int -> (a -> b) -> a -> IO ([Word64], b)
ticksLazy n0 f a = go f a n0 []
  where
    go f' a' n ts
      | n <= 0 = pure (ts, f a)
      | otherwise = do
        (t, _) <- tickLazy f a
        go f' a' (n - 1) (t:ts)

ticksInline :: Int -> (a -> b) -> a -> IO ([Word64], b)
ticksInline n0 f a = go f a n0 []
  where
    go f' a' n ts
      | n <= 0 = pure (ts, f a)
      | otherwise = do
        (t, _) <- tickInline f a
        go f' a' (n - 1) (t:ts)

ticksInlineable :: Int -> (a -> b) -> a -> IO ([Word64], b)
ticksInlineable n0 f a = go f a n0 []
  where
    go f' a' n ts
      | n <= 0 = pure (ts, f a)
      | otherwise = do
        (t, _) <- tickInlineable f a
        go f' a' (n - 1) (t:ts)

ticksNoinline :: Int -> (a -> b) -> a -> IO ([Word64], b)
ticksNoinline n0 f a = go f a n0 []
  where
    go f' a' n ts
      | n <= 0 = pure (ts, f a)
      | otherwise = do
        (t, _) <- tickNoinline f a
        go f' a' (n - 1) (t:ts)

ticksInline1 :: Int -> (a -> b) -> a -> IO ([Word64], b)
ticksInline1 n0 f a = go f a n0 []
  where
    go f' a' n ts
      | n <= 0 = pure (ts, f a)
      | otherwise = do
        (t, _) <- tickInline1 f a
        go f' a' (n - 1) (t:ts)

ticksInline2 :: Int -> (a -> b) -> a -> IO ([Word64], b)
ticksInline2 n0 f a = go f a n0 []
  where
    go f' a' n ts
      | n <= 0 = pure (ts, f a)
      | otherwise = do
        (t, _) <- tickInline2 f a
        go f' a' (n - 1) (t:ts)

ticksInline1' :: Int -> (a -> b) -> a -> IO ([Word64], b)
ticksInline1' n0 f a = go f a n0 []
  where
    go f' a' n ts
      | n <= 0 = pure (ts, f a)
      | otherwise = do
        (t, _) <- tickInline1' f a
        go f' a' (n - 1) (t:ts)

ticksInline2' :: Int -> (a -> b) -> a -> IO ([Word64], b)
ticksInline2' n0 f a = go f a n0 []
  where
    go f' a' n ts
      | n <= 0 = pure (ts, f a)
      | otherwise = do
        (t, _) <- tickInline2' f a
        go f' a' (n - 1) (t:ts)

runs :: (NFData a, NFData b) => Text -> (a -> b) -> a -> Int -> StatType -> IO ()
runs label f a n s = do
  putStrLn $ unpack label
  ticksNoPragma n f a & fmap (fst >>> stat s >>> T.unpack >>> ("ticksNoPragma " <>)) & (>>= putStrLn)
  ticksLazy n f a & fmap (fst >>> stat s >>> T.unpack >>> ("ticksLazy " <>)) & (>>= putStrLn)
  ticksInline n f a & fmap (fst >>> stat s >>> T.unpack >>> ("ticksInline " <>)) & (>>= putStrLn)
  ticksInlineable n f a & fmap (fst >>> stat s >>> T.unpack >>> ("ticksInlineable " <>)) & (>>= putStrLn)
  ticksNoinline n f a & fmap (fst >>> stat s >>> T.unpack >>> ("ticksNoinline " <>)) & (>>= putStrLn)
  ticksInline1 n f a & fmap (fst >>> stat s >>> T.unpack >>> ("ticksInline1 " <>)) & (>>= putStrLn)
  ticksInline2 n f a & fmap (fst >>> stat s >>> T.unpack >>> ("ticksInline2 " <>)) & (>>= putStrLn)
  ticksInline1' n f a & fmap (fst >>> stat s >>> T.unpack >>> ("ticksInline1' " <>)) & (>>= putStrLn)
  ticksInline2' n f a & fmap (fst >>> stat s >>> T.unpack >>> ("ticksInline2' " <>)) & (>>= putStrLn)
  multi tickNoPragma n f a & fmap (fst >>> stat s >>> T.unpack >>> ("multi tickNoPragma " <>)) & (>>= putStrLn)
  multi tickInline n f a & fmap (fst >>> stat s >>> T.unpack >>> ("multi tickInline " <>)) & (>>= putStrLn)
  multi tickInlineable n f a & fmap (fst >>> stat s >>> T.unpack >>> ("multi tickInlineable " <>)) & (>>= putStrLn)
  multi tickNoinline n f a & fmap (fst >>> stat s >>> T.unpack >>> ("multi tickNoinline " <>)) & (>>= putStrLn)
  multi tickInline1 n f a & fmap (fst >>> stat s >>> T.unpack >>> ("multi tickInline1 " <>)) & (>>= putStrLn)
  multi tickInline2 n f a & fmap (fst >>> stat s >>> T.unpack >>> ("multi tickInline2 " <>)) & (>>= putStrLn)
  multi tickInline1' n f a & fmap (fst >>> stat s >>> T.unpack >>> ("multi tickInline1' " <>)) & (>>= putStrLn)
  multi tickInline2' n f a & fmap (fst >>> stat s >>> T.unpack >>> ("multi tickInline2' " <>)) & (>>= putStrLn)
  ticks' n f a & reportStat ("ticks' " <> "" <> " | ") s

main :: IO ()
main = do
  o <- execParser opts
  let !n = optionRuns o
  let !l = optionLength o
  let !ls = [1..l]
  let s = optionStatType o
  let !a = optionAlgoType o
  _ <- warmup 100

  case a of
    AlgoFuseSum -> runs "fuseSum" fuseSum l n s
    AlgoFuseConst -> runs "fuseConst" fuseConst l n s
    AlgoRecSum -> runs "recSum" recSum ls n s
    AlgoMonoSum -> runs "monoSum" monoSum ls n s
    AlgoPolySum -> runs "polySum" polySum ls n s
    AlgoLambdaSum -> runs "lambdaSum" lambdaSum ls n s
