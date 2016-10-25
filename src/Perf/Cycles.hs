{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Perf.Cycles where

import Protolude
import System.CPUTime.Rdtsc
import Data.List
import qualified Control.Foldl as L

-- Cycles
type Cycles = Word64

-- | `tick f a` applies a to f, and strictly returns a (number of cycles, application result) tuple
tick :: (a -> b) -> a -> IO (Cycles, b)
tick f a = do
  t <- rdtsc
  !a' <- return (f a)
  t' <- rdtsc
  return (t' - t, a')

-- | variation that just acts on an `a`
tick' :: a -> IO (Cycles, a)
tick' a = do
  t <- rdtsc
  !a' <- return a
  t' <- rdtsc
  return (t' - t, a')

-- | variation that takes an `IO a`
tickM :: IO a -> IO (Cycles, a)
tickM a = do
  t <- rdtsc
  !a' <- a
  t' <- rdtsc
  return (t' - t, a')

-- | variation that just measures the number of cycles to take a tick measurement
tick_ :: IO Cycles
tick_ = do
  t <- rdtsc
  t' <- rdtsc
  return (t' - t)

-- | `tickf f a` applies a to f, and strictly returns a (number of cycles, application result) tuple, measuring just the f effect
tickf :: (a -> b) -> a -> IO (Cycles, b)
tickf f a = do
  !a' <- pure a
  t <- rdtsc
  !a'' <- return (f a')
  t' <- rdtsc
  return (t' - t, a'')

-- | monadic version
tickfM :: (a -> IO b) -> a -> IO (Cycles, b)
tickfM f a = do
  !a' <- pure a
  t <- rdtsc
  !a'' <- f a'
  t' <- rdtsc
  return (t' - t, a'')

-- | `ticka f a` applies a to f, and strictly returns a (number of cycles, application result) tuple, measuring just the a effect
ticka :: (a -> b) -> a -> IO (Cycles, b)
ticka f a = do
  t <- rdtsc
  !a' <- pure a
  t' <- rdtsc
  !a'' <- return (f a')
  return (t' - t, a'')

-- | `tickfa f a` applies a to f, and strictly returns a (number of cycles, application result) tuple, measuring both the f and the a effects separately.
tickfa :: (a -> b) -> a -> IO ((Cycles, Cycles), b)
tickfa f a = do
  t_a <- rdtsc
  !a' <- pure a
  t_a' <- rdtsc
  !a'' <- return (f a')
  t_f <- rdtsc
  return ((t_f - t_a', t_a' - t_a), a'')

-- | n measurements of whatever tick engine
spin :: Int -> ((a -> b) -> a -> IO (c, b)) ->
    (a -> b) -> a -> IO ([c], b)
spin n tick f a = do
    ticks <- replicateM n (tick f a)
    pure (fst <$> ticks, snd $ last ticks)

-- | n measurements of whatever tick engine
spinM :: Int -> ((a -> IO b) -> a -> IO (c, b)) ->
    (a -> IO b) -> a -> IO ([c], b)
spinM n tick f a = do
    ticks <- replicateM n (tick f a)
    pure (fst <$> ticks, snd $ last ticks)

-- | warm up the register, and the setup
warmup :: Int -> IO Double
warmup n = do
    ts <- replicateM n tick_
    pure $ average (fromIntegral <$> ts)
  where
    average cs = L.fold ((/) <$> L.sum <*> L.genericLength) cs

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
