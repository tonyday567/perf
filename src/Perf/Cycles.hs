{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Perf.Cycles where

import Protolude
import System.CPUTime.Rdtsc
import Data.List

-- Cycles
type Cycles = Word64

tick :: a -> IO (Cycles, a)
tick a = do
  t <- rdtsc
  !a' <- return a
  t' <- rdtsc
  return (t' - t, a')

tickM :: IO a -> IO (Cycles, a)
tickM a = do
  t <- rdtsc
  !a' <- a
  t' <- rdtsc
  return (t' - t, a')

tick_ :: IO Cycles
tick_ = do
  t <- rdtsc
  t' <- rdtsc
  return (t' - t)

tickn :: Int -> f -> IO ([Cycles], f)
tickn n f = do
    ticks <- replicateM n (tick f)
    pure (fst <$> ticks, snd $ last ticks)

-- conversion to a Double
cycles :: Cycles -> Double
cycles = fromIntegral

-- | helpers
force :: (NFData a) => a -> a
force x = x `deepseq` x

replicateM' :: Monad m
            => Int -> m a -> m [a]
replicateM' n op' = go n []
  where
    go 0 acc = return $ reverse acc
    go n' acc = do
        x <- op'
        go (n' - 1) (x : acc)
