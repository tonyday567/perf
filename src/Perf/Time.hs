{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | 'tick' uses the rdtsc chipset to measure time performance of a computation.
--
-- The measurement unit is one oscillation of the chip crystal as measured by the <https://en.wikipedia.org/wiki/Time_Stamp_Counter rdtsc> instruction which inspects the TSC register.
--
-- For reference, a computer with a frequency of 2 GHz means that one cycle is equivalent to 0.5 nanoseconds.
module Perf.Time
  ( -- * Usage
    -- $usage
    rdtsc,
    tick_,
    warmup,
    tick,
    tickWHNF,
    tickLazy,
    tickForce,
    tickForceArgs,
    tickIO,
    ticks,
    ticksIO,
    Cycles (..),
    cputime,
    clocktime,
    time,
    times,
    stepTime,
  )
where

import Control.DeepSeq
import Control.Monad (replicateM_)
import Data.Fixed
import Data.Time
import GHC.Word (Word64)
import Perf.Types
import System.CPUTime
import System.CPUTime.Rdtsc
import Prelude

-- $usage
-- > import Perf.Time
-- > import Data.Foldable (foldl')
-- > let r = 10000
-- > let l = 1000
-- > let f x = foldl' (+) 0 [1 .. x]
-- > first median <$> ticks r f a

newtype Cycles = Cycles {word :: Word64}
  deriving (Show, Read, Eq, Ord, Num, Real, Enum, Integral)

instance Semigroup Cycles where
  (<>) = (+)

instance Monoid Cycles where
  mempty = 0

-- | tick_ measures the number of cycles it takes to read the rdtsc chip twice: the difference is then how long it took to read the clock the second time.
tick_ :: IO Cycles
tick_ = do
  t <- rdtsc
  t' <- rdtsc
  pure (Cycles (t' - t))

-- | Warm up the register, to avoid a high first measurement. Without a warmup, one or more larger values can occur at the start of a measurement spree, and often are in the zone of an L2 miss.
warmup :: Int -> IO ()
warmup n = replicateM_ n tick_

-- | `tick f a`
--
-- - strictly evaluates f and a to WHNF
-- - starts the cycle counter
-- - strictly evaluates f a to WHNF
-- - stops the cycle counter
-- - returns (number of cycles, f a)
tick :: (a -> b) -> a -> IO (Cycles, b)
tick !f !a = do
  !t <- rdtsc
  !a' <- pure $! f a
  !t' <- rdtsc
  pure (Cycles (t' - t), a')
{-# INLINEABLE tick #-}

-- | `tickWHNF f a`
--
-- - starts the cycle counter
-- - strictly evaluates f a to WHNF (this may also kick off thunk evaluation in f or a which will also be captured in the cycle count)
-- - stops the cycle counter
-- - returns (number of cycles, f a)
tickWHNF :: (a -> b) -> a -> IO (Cycles, b)
tickWHNF f a = do
  !t <- rdtsc
  !a' <- pure $! f a
  !t' <- rdtsc
  pure (Cycles (t' - t), a')
{-# INLINEABLE tickWHNF #-}

-- | `tickLazy f a`
--
-- - starts the cycle counter
-- - lazily evaluates f a
-- - stops the cycle counter
-- - returns (number of cycles, f a)
tickLazy :: (a -> b) -> a -> IO (Cycles, b)
tickLazy f a = do
  t <- rdtsc
  let a' = f a
  t' <- rdtsc
  pure (Cycles (t' - t), a')
{-# INLINEABLE tickLazy #-}

-- | `tickForce f a`
--
-- - deeply evaluates f and a,
-- - starts the cycle counter
-- - deeply evaluates f a
-- - stops the cycle counter
-- - returns (number of cycles, f a)
tickForce :: (NFData a, NFData b) => (a -> b) -> a -> IO (Cycles, b)
tickForce (force -> !f) (force -> !a) = do
  !t <- rdtsc
  !a' <- pure (force (f a))
  !t' <- rdtsc
  pure (Cycles (t' - t), a')
{-# INLINEABLE tickForce #-}

-- | `tickForceArgs f a`
--
-- - deeply evaluates f and a,
-- - starts the cycle counter
-- - strictly evaluates f a to WHNF
-- - stops the cycle counter
-- - returns (number of cycles, f a)
tickForceArgs :: (NFData a) => (a -> b) -> a -> IO (Cycles, b)
tickForceArgs (force -> !f) (force -> !a) = do
  !t <- rdtsc
  !a' <- pure $! f a
  !t' <- rdtsc
  pure (Cycles (t' - t), a')
{-# INLINEABLE tickForceArgs #-}

-- | measures an `IO a`
tickIO :: IO a -> IO (Cycles, a)
tickIO a = do
  !t <- rdtsc
  !a' <- a
  !t' <- rdtsc
  pure (Cycles (t' - t), a')
{-# INLINEABLE tickIO #-}

-- | n measurements of a tick
--
-- returns a list of Cycles and the last evaluated f a
ticks :: Int -> (a -> b) -> a -> IO ([Cycles], b)
ticks = multi tick
{-# INLINEABLE ticks #-}

-- | n measurements of a tickIO
--
-- returns an IO tuple; list of Cycles and the last evaluated f a
ticksIO :: Int -> IO a -> IO ([Cycles], a)
ticksIO = multiM tickIO
{-# INLINEABLE ticksIO #-}

stepTime :: StepMeasure IO Cycles
stepTime = StepMeasure start stop
  where
    start = Cycles <$> rdtsc
    stop r = fmap (\x -> x - r) (Cycles <$> rdtsc)
{-# INLINEABLE stepTime #-}

-- | a measure using 'getCPUTime' from System.CPUTime (unit is picoseconds)
cputime :: StepMeasure IO Integer
cputime = StepMeasure start stop
  where
    start = getCPUTime
    stop a = do
      t <- getCPUTime
      return $ t - a

-- | a measure using 'getCurrentTime' (unit is seconds)
clocktime :: StepMeasure IO Double
clocktime = StepMeasure start stop
  where
    start = getCurrentTime
    stop a = do
      t <- getCurrentTime
      return $ fromNominalDiffTime $ diffUTCTime t a

fromNominalDiffTime :: NominalDiffTime -> Double
fromNominalDiffTime t = fromInteger i * 1e-12
  where
    (MkFixed i) = nominalDiffTimeToSeconds t

time :: Measure IO Cycles
time = Measure tick tickIO
{-# INLINEABLE time #-}

times :: Int -> Measure IO [Cycles]
times n = Measure (ticks n) (ticksIO n)
{-# INLINEABLE times #-}
