{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | 'tick' uses the rdtsc chipset to measure time performance of a computation.
--
-- The measurement unit - a 'Cycle' - is one oscillation of the chip crystal as measured by the <https://en.wikipedia.org/wiki/Time_Stamp_Counter rdtsc> instruction which inspects the TSC register.
--
-- For reference, a computer with a frequency of 2 GHz means that one cycle is equivalent to 0.5 nanoseconds.
module Perf.Cycle
  ( -- * Usage

    -- $usage
    rdtsc,
    Cycle,
    tick_,
    warmup,
    tick,
    tickUnsafe,
    tickIO,
    multi,
    ticks,
    ticksIO,
    average,
    median,
    tenth,
  )
where

import Control.Monad (replicateM_, replicateM)
import GHC.Word (Word64)
import System.CPUTime.Rdtsc
import Prelude
import NumHask.Space (quantile)
import Data.Text (Text)
import Data.FormatN


-- $usage
-- > import Perf.Cycle
-- > import Data.Foldable (foldl')
-- > let r = 10000
-- > let l = 1000
-- > let f x = foldl' (+) 0 [1 .. x]
-- > first median <$> ticks r f a
--

-- | an unwrapped Word64
type Cycle = Word64

-- | tick_ measures the number of cycles it takes to read the rdtsc chip twice: the difference is then how long it took to read the clock the second time.
--
-- Below are indicative measurements using tick_:
--
-- >>> onetick <- tick_
-- >>> ticks' <- replicateM 10 tick_
-- >>> manyticks <- replicateM 1000000 tick_
--
-- The distribution of tick_ measurements is highly skewed, with the maximum being around 50k cycles, which is of the order of a GC. The important point on the distribution is around the 30th to 50th percentile, where you get a clean measure, usually free of GC activity and cache miss-fires
tick_ :: IO Cycle
tick_ = do
  t <- rdtsc
  t' <- rdtsc
  pure (t' - t)

-- | Warm up the register, to avoid a high first measurement. Without a warmup, one or more larger values can occur at the start of a measurement spree, and often are in the zone of an L2 miss.
--
-- >>> t <- tick_ -- first measure can be very high
-- >>> _ <- warmup 100
-- >>> t <- tick_ -- should be around 20 (3k for ghci)
warmup :: Int -> IO ()
warmup n = replicateM_ n tick_

-- | This is unsafe from the point of view of being prone to memoization via being inlined.
--

-- | `tick f a` strictly evaluates f and a, then evaluates f a, returning a (Cycle, f a)
--
-- Noinline pragma is applied to prevent memoization.
--
-- > (cycleList, result) <- tick f a
--
tick :: (a -> b) -> a -> IO (Cycle, b)
tick !f !a = do
  !t <- rdtsc
  !a' <- pure (f a)
  !t' <- rdtsc
  pure (t' - t, a')
{-# NOINLINE tick #-}

-- | This is unsafe from the point of view of being prone to memoization via being inlined upstream.
--
tickUnsafe :: (a -> b) -> a -> IO (Cycle, b)
tickUnsafe !f !a = do
  !t <- rdtsc
  !a' <- pure (f a)
  !t' <- rdtsc
  pure (t' - t, a')
{-# INLINEABLE tickUnsafe #-}

-- | measures an `IO a`
--
-- >>> (cs, _) <- tickIO (pure (f a))
tickIO :: IO a -> IO (Cycle, a)
tickIO a = do
  t <- rdtsc
  !a' <- a
  t' <- rdtsc
  pure (t' - t, a')
{-# INLINEABLE tickIO #-}

multi :: ((a -> b) -> a -> IO (Cycle, b)) -> Int -> (a -> b) -> a -> IO ([Cycle], b)
multi tickf n0 f a = fmap (\xs -> (fmap fst xs, snd (head xs))) (replicateM n0 (tickf f a))
{-# INLINE multi #-}

-- | n measurements of a tick
--
-- returns a list of Cycles and the last evaluated f a
--
ticks :: Int -> (a -> b) -> a -> IO ([Cycle], b)
ticks = multi tick
{-# INLINEABLE ticks #-}

-- | n measurements of a tickIO
--
-- returns an IO tuple; list of Cycles and the last evaluated f a
--
-- >>> (cs, fa) <- ticksIO n (pure $ f a)
ticksIO :: Int -> IO a -> IO ([Cycle], a)
ticksIO n0 a = fmap (\xs -> (fmap fst xs, snd (head xs))) (replicateM n0 (tickIO a))
{-# INLINEABLE ticksIO #-}

median :: [Cycle] -> Text
median = comma (Just 3) . quantile 0.5 . fmap Prelude.fromIntegral

average :: [Cycle] -> Text
average = comma (Just 3) . (\xs -> (fromIntegral . Prelude.toInteger . sum $ xs) / (fromIntegral . length $ xs))

tenth :: [Cycle] -> Text
tenth = comma (Just 3) . quantile 0.1 . fmap Prelude.fromIntegral
