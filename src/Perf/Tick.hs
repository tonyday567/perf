{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ViewPatterns #-}

-- | 'tick' uses the rdtsc chipset to measure time performance of a computation.
--
-- The measurement unit is one oscillation of the chip crystal as measured by the <https://en.wikipedia.org/wiki/Time_Stamp_Counter rdtsc> instruction which inspects the TSC register.
--
-- For reference, a computer with a frequency of 2 GHz means that one cycle is equivalent to 0.5 nanoseconds.
module Perf.Tick
  ( -- * Usage

    -- $usage
    count_,
    warmup,
    count,
    countWHNF,
    countLazy,
    countForce,
    countForceArgs,
    countIO,
    multiple,
    counts,
    countsIO,

    Word64,
  )
where

import Control.Monad (replicateM_, replicateM)
import GHC.Word (Word64)
import System.CPUTime.Rdtsc
import Prelude
import Control.DeepSeq

-- $usage
-- > import Perf.Tick
-- > import Data.Foldable (foldl')
-- > let r = 10000
-- > let l = 1000
-- > let f x = foldl' (+) 0 [1 .. x]
-- > first median <$> counts r f a
--

-- | count_ measures the number of cycles it takes to read the rdtsc chip twice: the difference is then how long it took to read the clock the second time.
--
-- Below are indicative measurements using count_:
--
-- >>> onecount <- count_
-- >>> counts' <- replicateM 10 count_
-- >>> manycounts <- replicateM 1000000 count_
--
-- The distribution of count_ measurements is highly skewed, with the maximum being around 50k cycles, which is of the order of a GC. The important point on the distribution is around the 30th to 50th percentile, where you get a clean measure, usually free of GC activity and cache miss-fires
count_ :: IO Word64
count_ = do
  t <- rdtsc
  t' <- rdtsc
  pure (t' - t)

-- | Warm up the register, to avoid a high first measurement. Without a warmup, one or more larger values can occur at the start of a measurement spree, and often are in the zone of an L2 miss.
--
-- >>> t <- count_ -- first measure can be very high
-- >>> _ <- warmup 100
-- >>> t <- count_ -- should be around 20 (3k for ghci)
warmup :: Int -> IO ()
warmup n = replicateM_ n count_

-- | `count f a`
--
-- - strictly evaluates f and a to WHNF
-- - starts the cycle counter
-- - strictly evaluates f a to WHNF
-- - stops the cycle counter
-- - returns (number of cycles, f a)
--
count :: (a -> b) -> a -> IO (Word64, b)
count !f !a = do
  !t <- rdtsc
  !a' <- pure $! f a
  !t' <- rdtsc
  pure (t' - t, a')
{-# INLINEABLE count #-}

-- | `countWHNF f a`
--
-- - starts the cycle counter
-- - strictly evaluates f a to WHNF (this may also kick off thunk evaluation in f or a which will also be captured in the cycle count)
-- - stops the cycle counter
-- - returns (number of cycles, f a)
--
countWHNF :: (a -> b) -> a -> IO (Word64, b)
countWHNF f a = do
  !t <- rdtsc
  !a' <- pure $! f a
  !t' <- rdtsc
  pure (t' - t, a')
{-# INLINEABLE countWHNF #-}

-- | `countLazy f a`
--
-- - starts the cycle counter
-- - lazily evaluates f a to WHNF
-- - stops the cycle counter
-- - returns (number of cycles, f a)
--
countLazy :: (a -> b) -> a -> IO (Word64, b)
countLazy f a = do
  t <- rdtsc
  let a' = f a
  t' <- rdtsc
  pure (t' - t, a')
{-# INLINEABLE countLazy #-}

-- | `countForce f a`
--
-- - deeply evaluates f and a,
-- - starts the cycle counter
-- - deeply evaluates f a
-- - stops the cycle counter
-- - returns (number of cycles, f a)
--
countForce :: (NFData a, NFData b) => (a -> b) -> a -> IO (Word64, b)
countForce (force -> !f) (force -> !a) = do
  !t <- rdtsc
  !a' <- pure (force (f a))
  !t' <- rdtsc
  pure (t' - t, a')
{-# INLINEABLE countForce #-}

-- | `countForceArgs f a`
--
-- - deeply evaluates f and a,
-- - starts the cycle counter
-- - strictly evaluates f a to WHNF
-- - stops the cycle counter
-- - returns (number of cycles, f a)
--
countForceArgs :: (NFData a) => (a -> b) -> a -> IO (Word64, b)
countForceArgs (force -> !f) (force -> !a) = do
  !t <- rdtsc
  !a' <- pure $! f a
  !t' <- rdtsc
  pure (t' - t, a')
{-# INLINEABLE countForceArgs #-}

-- | measures an `IO a`
--
-- >>> (cs, _) <- countIO (pure (f a))
countIO :: IO a -> IO (Word64, a)
countIO a = do
  !t <- rdtsc
  !a' <- a
  !t' <- rdtsc
  pure (t' - t, a')
{-# INLINEABLE countIO #-}

multiple :: ((a -> b) -> a -> IO (Word64, b)) -> Int -> (a -> b) -> a -> IO ([Word64], b)
multiple countf n0 f a = fmap (\xs -> (fmap fst xs, snd (head xs))) (replicateM n0 (countf f a))
{-# INLINEABLE multiple #-}

-- | n measurements of a count
--
-- returns a list of Word64s and the last evaluated f a
--
counts :: Int -> (a -> b) -> a -> IO ([Word64], b)
counts = multiple count
{-# INLINEABLE counts #-}

-- | n measurements of a countIO
--
-- returns an IO tuple; list of Word64s and the last evaluated f a
--
-- >>> (cs, fa) <- countsIO n (pure $ f a)
countsIO :: Int -> IO a -> IO ([Word64], a)
countsIO n0 a = fmap (\xs -> (fmap fst xs, snd (head xs))) (replicateM n0 (countIO a))
{-# INLINEABLE countsIO #-}
