{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

-- | Use of 'System.Clock' from the [clock](https://hackage.haskell.org/package/clock) library to measure time performance of a computation.
module Perf.Time
  ( Nanos,
    defaultClock,
    toSecs,
    nanosWith,
    nanos,
    tick_,
    warmup,
    tickWith,
    tick,
    tickWHNF,
    tickLazy,
    tickForce,
    tickForceArgs,
    tickIO,
    tickIOWith,
    ticks,
    ticksIO,
    time,
    times,
    timesWith,
    timesN,
    timesNWith,
    stepTime,
  )
where

import Control.DeepSeq
import Control.Monad (replicateM_)
import Perf.Types
import System.Clock
import Prelude

-- | A performance measure of number of nanoseconds.
type Nanos = Integer

-- | Convert 'Nanos' to seconds.
toSecs :: Nanos -> Double
toSecs ns = fromIntegral ns / 1e9

-- | 'MonotonicRaw' is the default for macOS & linux, at around 42 nano time resolution, and a 'tick_' measurement of around 170 nanos. For Windows, 'ThreadCPUTime' has a similar time resolution at 42 nanos and a 'tick_' of around 500 nanos.
defaultClock :: Clock

#ifdef mingw32_HOST_OS
defaultClock = ThreadCPUTime
#else
defaultClock = MonotonicRaw
#endif

-- | A single 'defaultClock' reading (note that the absolute value is not meaningful).
nanos :: IO Nanos
nanos = nanosWith defaultClock

-- | A single reading of a specific 'Clock'.
nanosWith :: Clock -> IO Nanos
nanosWith c = toNanoSecs <$> getTime c

-- | tick_ measures the number of nanos it takes to read the clock.
tick_ :: IO Nanos
tick_ = do
  t <- nanos
  t' <- nanos
  pure (t' - t)

-- | Warm up the clock, to avoid a high first measurement. Without a warmup, one or more larger values can occur at the start of a measurement spree, and often are in the zone of an L2 miss.
warmup :: Int -> IO ()
warmup n = replicateM_ n tick_

-- | tick from a specific 'Clock'
tickWith :: Clock -> (a -> b) -> a -> IO (Nanos, b)
tickWith c !f !a = do
  !t <- nanosWith c
  !a' <- pure $! f a
  !t' <- nanosWith c
  pure (t' - t, a')
{-# INLINEABLE tickWith #-}

-- | /tick f a/
--
-- - strictly evaluates f and a to WHNF
-- - reads the clock
-- - strictly evaluates f a to WHNF
-- - reads the clock
-- - returns (nanos, f a)
tick :: (a -> b) -> a -> IO (Nanos, b)
tick !f !a = do
  !t <- nanos
  !a' <- pure $! f a
  !t' <- nanos
  pure (t' - t, a')
{-# INLINEABLE tick #-}

-- | /tickWHNF f a/
--
-- - reads the clock
-- - strictly evaluates f a to WHNF (this may also kick off thunk evaluation in f or a which will also be captured in the cycle count)
-- - reads the clock
-- - returns (nanos, f a)
tickWHNF :: (a -> b) -> a -> IO (Nanos, b)
tickWHNF f a = do
  !t <- nanos
  !a' <- pure $! f a
  !t' <- nanos
  pure (t' - t, a')
{-# INLINEABLE tickWHNF #-}

-- | /tickLazy f a/
--
-- - reads the clock
-- - lazily evaluates f a
-- - reads the clock
-- - returns (nanos, f a)
tickLazy :: (a -> b) -> a -> IO (Nanos, b)
tickLazy f a = do
  t <- nanos
  let a' = f a
  t' <- nanos
  pure (t' - t, a')
{-# INLINEABLE tickLazy #-}

-- | /tickForce f a/
--
-- - deeply evaluates f and a,
-- - reads the clock
-- - deeply evaluates f a
-- - reads the clock
-- - returns (nanos, f a)
tickForce :: (NFData a, NFData b) => (a -> b) -> a -> IO (Nanos, b)
tickForce (force -> !f) (force -> !a) = do
  !t <- nanos
  !a' <- pure (force (f a))
  !t' <- nanos
  pure (t' - t, a')
{-# INLINEABLE tickForce #-}

-- | /tickForceArgs f a/
--
-- - deeply evaluates f and a,
-- - reads the clock
-- - strictly evaluates f a to WHNF
-- - reads the clock
-- - returns (nanos, f a)
tickForceArgs :: (NFData a) => (a -> b) -> a -> IO (Nanos, b)
tickForceArgs (force -> !f) (force -> !a) = do
  !t <- nanos
  !a' <- pure $! f a
  !t' <- nanos
  pure (t' - t, a')
{-# INLINEABLE tickForceArgs #-}

-- | measures an /IO a/
tickIO :: IO a -> IO (Nanos, a)
tickIO a = do
  !t <- nanos
  !a' <- a
  !t' <- nanos
  pure (t' - t, a')
{-# INLINEABLE tickIO #-}

-- | measures an /IO a/
tickIOWith :: Clock -> IO a -> IO (Nanos, a)
tickIOWith c a = do
  !t <- nanosWith c
  !a' <- a
  !t' <- nanosWith c
  pure (t' - t, a')
{-# INLINEABLE tickIOWith #-}

-- | n measurements of a tick
--
-- returns a list of Nanos and the last evaluated f a
ticks :: Int -> (a -> b) -> a -> IO ([Nanos], b)
ticks = multi tick
{-# INLINEABLE ticks #-}

-- | n measurements of a tickIO
--
-- returns an IO tuple; list of Nanos and the last evaluated f a
ticksIO :: Int -> IO a -> IO ([Nanos], a)
ticksIO = multiM tickIO
{-# INLINEABLE ticksIO #-}

-- | tick as a 'StepMeasure'
stepTime :: StepMeasure IO Nanos
stepTime = StepMeasure start stop
  where
    start = nanos
    stop r = fmap (\x -> x - r) nanos
{-# INLINEABLE stepTime #-}

-- | tick as a 'Measure'
time :: Measure IO Nanos
time = Measure tick
{-# INLINEABLE time #-}

-- | tick as a multi-Measure
times :: Int -> Measure IO [Nanos]
times n = Measure (ticks n)
{-# INLINEABLE times #-}

-- | tickWith as a multi-Measure
timesWith :: Clock -> Int -> Measure IO [Nanos]
timesWith c n = repeated n (Measure (tickWith c))
{-# INLINEABLE timesWith #-}

-- | tickWith for n repeated applications
timesN :: Int -> Measure IO Nanos
timesN n = Measure (tickNWith defaultClock n)
{-# INLINEABLE timesN #-}

-- | tickWith for n repeated applications
timesNWith :: Clock -> Int -> Measure IO Nanos
timesNWith c n = Measure (tickNWith c n)
{-# INLINEABLE timesNWith #-}

tickNWith :: Clock -> Int -> (a -> b) -> a -> IO (Nanos, b)
tickNWith c n !f !a = do
  !t <- nanosWith c
  !a' <- multiN id f a n
  !t' <- nanosWith c
  pure (floor @Double (fromIntegral (t' - t) / fromIntegral n), a')
{-# INLINEABLE tickNWith #-}
