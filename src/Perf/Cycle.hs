{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | 'tick' uses the rdtsc chipset to measure time performance of a computation.
--
-- The measurement unit - a 'Cycle' - is one oscillation of the chip crystal as measured by the <https://en.wikipedia.org/wiki/Time_Stamp_Counter rdtsc> instruction which inspects the TSC register.
--
-- For reference, a computer with a frequency of 2 GHz means that one cycle is equivalent to 0.5 nanoseconds.
--
module Perf.Cycle
  ( -- $setup
    Cycle
  , tick_
  , warmup
  , tick
  , app
  , tickIO
  , ticks
  , qtick
  , ticksIO
  , tickns
  , replicateM'
  , average
  , deciles
  , percentile
  ) where

import qualified Control.Foldl as L
import Data.List
import Data.TDigest
import NumHask.Prelude
import System.CPUTime.Rdtsc
import qualified Protolude

-- $setup
-- >>> :set -XNoImplicitPrelude
-- >>> import Perf.Cycle
-- >>> let n = 1000
-- >>> let a = 1000
-- >>> let f x = foldl' (+) 0 [1 .. x]
--


-- | an unwrapped Word64
type Cycle = Word64

instance AdditiveMagma Cycle where
  plus = (Protolude.+)

instance AdditiveUnital Cycle where
  zero = 0

instance AdditiveAssociative Cycle

instance AdditiveCommutative Cycle

instance Additive Cycle

instance AdditiveInvertible Cycle where
  negate = Protolude.negate

instance AdditiveGroup Cycle

instance ToInteger Cycle where
    toInteger = Protolude.toInteger

-- | tick_ measures the number of cycles it takes to read the rdtsc chip twice: the difference is then how long it took to read the clock the second time.
--
-- Below are indicative measurements using tick_:
--
-- >>> onetick <- tick_
-- >>> ticks' <- replicateM 10 tick_
-- >>> manyticks <- replicateM 1000000 tick_
-- >>> let average = L.fold ((/) <$> L.sum <*> L.genericLength)
-- >>> let avticks = average (fromIntegral <$> manyticks)
-- >>> let qticks = deciles 10 manyticks
-- >>> let tick999 = percentile 0.999 manyticks
--
-- > one tick_: 78 cycles
-- > next 10: [20,18,20,20,20,20,18,16,20,20]
-- > average over 1m: 20.08 cycles
-- > 99.999% perc: 7,986
-- > 99.9% perc: 50.97
-- > 99th perc:  24.99
-- > 40th perc:  18.37
-- > [min, 10th, 20th, .. 90th, max]:
-- > 12.00 16.60 17.39 17.88 18.37 18.86 19.46 20.11 20.75 23.04 5.447e5
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
--
warmup :: Int -> IO Double
warmup n = do
  ts <- replicateM n tick_
  pure $ average ts

-- | `tick f a` strictly applies a to f, and returns a (Cycle, f a)
--
-- >>> _ <- warmup 100
-- >>> (cs, _) <- tick f a
--
-- > one tick: 197012 cycles
-- > average over 1000: 10222.79 cycles -- 10 cycles per operation
-- > [min, 30th, median, 90th, 99th, max]:
-- > 1.002e4 1.011e4 1.013e4 1.044e4 1.051e4 2.623e4
tick :: (NFData b) => (a -> b) -> a -> IO (Cycle, b)
tick f a = do
  !t <- rdtsc
  !a' <- pure (f a)
  !t' <- rdtsc
  pure (t' - t, a')

-- | evaluates and measures an `IO a`
--
-- >>> (cs, _) <- tickIO (pure (f a))
--
tickIO :: IO a -> IO (Cycle, a)
tickIO a = do
  t <- rdtsc
  !a' <- a
  t' <- rdtsc
  pure (t' - t, a')

-- | needs more testing
app :: t -> () -> t
app e () = e
{-# NOINLINE app #-}

-- | n measurements of a tick
--
-- returns a list of Cycles and the last evaluated f a
--
-- GHC is very good as memoization, and any of the functions that measuring a computation multiple times are fraught.  When a computation actually gets memoized is an inexact science.  Current readings are:
--
-- > sum to 1000.0
-- > Perf.ticks n f a                        8.37e3 cycles
-- > Main.ticks n f a                        8.38e3 cycles
-- > Perf.ticksIO n (pure $ f a)             8.38e3 cycles
-- > Perf.qtick n f a                        8.38e3 cycles
-- > Main.qtick n f a                        8.38e3 cycles
-- > replicateM n (tick f a)                 8.37e3 cycles
-- > replicateM' n (tick f a)                9.74e3 cycles
-- > replicateM n (tickIO (pure (f a)))      1.21e4 cycles
-- > replicateM n (tick (app (f a)) ())      9.72e3 cycles
-- > replicateM n (tick identity (f n))      18.2 cycles
-- > replicateM n (tick (const (f a)) ())    9.71e3 cycles
-- > (replicateM n . tick f) <$> [1,10,100,1000,10000]:  16.3 16.2 16.3 16.2 16.2
-- > Perf.tickns n f [1,10,100,1000,10000]:  16.2 16.2 16.2 16.2 16.2
--
-- >>> let n = 1000
-- >>> (cs, fa) <- ticks n f a
--
ticks :: (NFData b) => Int -> (a -> b) -> a -> IO ([Cycle], b)
ticks n f a = do
  ts <- replicateM' n (tick f a)
  pure (fst <$> ts, snd $ last ts)
{-# INLINE ticks #-}

-- | returns the 40th percentile measurement and the last evaluated f a
--
-- >>> (c, fa) <- qtick n f a
--
qtick :: (NFData b) => Int -> (a -> b) -> a -> IO (Double, b)
qtick n f a = do
  ts <- replicateM' n (tick f a)
  pure (percentile 0.4 $ fst <$> ts, snd $ last ts)
{-# INLINE qtick #-}

-- | n measuremenst of a tickIO
--
-- returns an IO tuple; list of Cycles and the last evaluated f a
--
-- >>> (cs, fa) <- ticksIO n (pure $ f a)
--
ticksIO :: Int -> IO a -> IO ([Cycle], a)
ticksIO n a = do
    cs <- replicateM n (tickIO a)
    pure (fst <$> cs, last $ snd <$> cs)

-- | n measurements on each of a list of a's to be applied to f.
--
-- Currently memoizing it's ass off
--
-- > tickns n f [1,10,100,1000]
--
tickns :: (NFData b) => Int -> (a -> b) -> [a] -> IO ([[Cycle]], [b])
tickns n f as = do
  cs <- sequence $ ticks n f <$> as
  pure (fst <$> cs, snd <$> cs)

-- | a replicateM with good attributes
replicateM' :: Monad m => Int -> m a -> m [a]
replicateM' n op' = go n []
  where
    go 0 acc = return $ reverse acc
    go n' acc = do
      x <- op'
      go (n' - 1) (x : acc)

-- | average of a Cycle foldable
--
-- > cAv <- average <$> ticks n f a
--
average :: (Foldable f) => f Cycle -> Double
average = L.fold (L.premap fromIntegral ((/) <$> L.sum <*> L.genericLength))

-- | compute deciles
--
-- > c5 <- decile 5 <$> ticks n f a
--
deciles :: (Functor f, Foldable f) => Int -> f Cycle -> [Double]
deciles n xs =
  (\x -> fromMaybe 0 $ quantile x (tdigest (fromIntegral <$> xs) :: TDigest 25)) <$>
  ((/ fromIntegral n) . fromIntegral <$> [0 .. n]) :: [Double]

-- | compute a percentile
--
-- > c <- percentoile 0.4 <$> ticks n f a
--
percentile :: (Functor f, Foldable f) => Double -> f Cycle -> Double
percentile p xs = fromMaybe 0 $ quantile p (tdigest (fromIntegral <$> xs) :: TDigest 25)

