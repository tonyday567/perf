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
  , tick'
  , tickIO
  , ticks
  , ticksIO
  , ns
  , tickWHNF
  , tickWHNF'
  , tickWHNFIO
  , ticksWHNF
  , ticksWHNFIO
  , average
  , deciles
  , percentile
  ) where

import qualified Control.Foldl as L
import Data.List as List
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

-- | tick where the arguments are lazy, so measurement may include evluation of thunks that may constitute f and/or a
tick' :: (NFData b) => (a -> b) -> a -> IO (Cycle, b)
tick' f a = do
  !t <- rdtsc
  !a' <- pure (force $ f a)
  !t' <- rdtsc
  pure (t' - t, a')

-- | `tick f a` strictly evaluates f and a, then deeply evaluates f a, returning a (Cycle, f a)
--
-- >>> _ <- warmup 100
-- >>> (cs, _) <- tick f a
--
-- > sum to 1000
-- > first measure: 1202 cycles
-- > second measure: 18 cycles
--
-- Note that feeding the same computation through tick twice will tend to kick off sharing (aka memoization aka let floating).  Given the importance of sharing to GHC optimisations this is the intended behaviour.  If you want to turn this off then see -fn--full-laziness (and maybe -fno-cse).
tick :: (NFData b) => (a -> b) -> a -> IO (Cycle, b)
tick !f !a = tick' f a

tickNoinline :: (NFData b) => (a -> b) -> a -> IO (Cycle, b)
tickNoinline !f !a = tick' f a
{-# NOINLINE tickNoinline #-}

-- | measures and deeply evaluates an `IO a`
--
-- >>> (cs, _) <- tickIO (pure (f a))
--
tickIO :: (NFData a) => IO a -> IO (Cycle, a)
tickIO a = do
  t <- rdtsc
  !a' <- force <$> a
  t' <- rdtsc
  pure (t' - t, a')

tickIONoinline :: (NFData a) => IO a -> IO (Cycle, a)
tickIONoinline = tickIO
{-# NOINLINE tickIONoinline #-}

-- | n measurements of a tick
--
-- returns a list of Cycles and the last evaluated f a
--
-- GHC is very good at finding ways to share computation, and anything measuring a computation multiple times is a prime candidate for aggresive ghc treatment. Internally, ticks uses a noinline pragma and a noinline on tick to help reduce the chances of memoization, but this is an inexact science in the hands of he author, at least, so interpret with caution.
--
-- 
-- >>> let n = 1000
-- >>> (cs, fa) <- ticks n f a
--
-- Baseline speed can be highly senistive to the nature of the function trimmings.  Polymorphic functions can tend to be slightly slower, and functions with lambda expressions can experience dramatic slowdowns.
--
-- > fMono :: Int -> Int
-- > fMono x = foldl' (+) 0 [1 .. x]
-- > fPoly :: (Enum b, Num b, Additive b) => b -> b
-- > fPoly x = foldl' (+) 0 [1 .. x]
-- > fLambda :: Int -> Int
-- > fLambda = \x -> foldl' (+) 0 [1 .. x]
--
-- > sum to 1000 n = 1000 prime run: 1.13e3
-- > run                       first     2nd     3rd     4th     5th  40th %
-- > ticks                    1.06e3     712     702     704     676    682 cycles
-- > ticks (lambda)           1.19e3     718     682     684     678    682 cycles
-- > ticks (poly)             1.64e3  1.34e3  1.32e3  1.32e3  1.32e3 1.31e3 cycles
--
ticks :: (NFData b) => Int -> (a -> b) -> a -> IO ([Cycle], b)
ticks n0 f a = go f a n0 []
  where
    go f' a' n ts
      | n <= 0 = pure (reverse ts, f a)
      | otherwise = do
          (t,_) <- tickNoinline f a
          go f' a' (n - 1) (t:ts)
{-# NOINLINE ticks #-}

-- | n measuremenst of a tickIO
--
-- returns an IO tuple; list of Cycles and the last evaluated f a
--
-- >>> (cs, fa) <- ticksIO n (pure $ f a)
--
-- > ticksIO                     834     752     688     714     690    709 cycles
-- > ticksIO (lambda)            822     690     720     686     688    683 cycles
-- > ticksIO (poly)           1.01e3     688     684     682     712    686 cycles
ticksIO :: (NFData a) => Int -> IO a -> IO ([Cycle], a)
ticksIO n0 a = go a n0 []
  where
    go a' n ts
      | n <= 0 = do
            a'' <- a'
            pure (reverse ts, a'')
      | otherwise = do
          (t,_) <- tickIONoinline a'
          go a' (n - 1) (t:ts)
{-# NOINLINE ticksIO #-}

-- | make a series of measurements on a list of a's to be applied to f, for a tick function.
--
-- Tends to be fragile to sharing issues, but very useful to determine computation Order
--
-- > ns ticks n f [1,10,100,1000]
--
-- > sum to's [1,10,100,1000]
-- > tickns n fMono:  17.8 23.5 100 678
--
ns :: (NFData b) => (a -> IO ([Cycle],b)) -> [a] -> IO ([[Cycle]], [b])
ns t as = do
  cs <- sequence $ t <$> as
  pure (fst <$> cs, snd <$> cs)

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
-- > c <- percentile 0.4 <$> ticks n f a
--
percentile :: (Functor f, Foldable f) => Double -> f Cycle -> Double
percentile p xs = fromMaybe 0 $ quantile p (tdigest (fromIntegral <$> xs) :: TDigest 25)


-- | WHNF version
tickWHNF :: (a -> b) -> a -> IO (Cycle, b)
tickWHNF !f !a = tickWHNF' f a

tickWHNFNoinline :: (a -> b) -> a -> IO (Cycle, b)
tickWHNFNoinline !f !a = tickWHNF' f a
{-# NOINLINE tickWHNFNoinline #-}

-- | WHNF version
tickWHNF' :: (a -> b) -> a -> IO (Cycle, b)
tickWHNF' f a = do
  !t <- rdtsc
  !a' <- pure (f a)
  !t' <- rdtsc
  pure (t' - t, a')

-- | WHNF version
tickWHNFIO :: IO a -> IO (Cycle, a)
tickWHNFIO a = do
  t <- rdtsc
  !a' <- a
  t' <- rdtsc
  pure (t' - t, a')

tickWHNFIONoinline :: IO a -> IO (Cycle, a)
tickWHNFIONoinline = tickWHNFIO
{-# NOINLINE tickWHNFIONoinline #-}

-- | WHNF version
ticksWHNF :: Int -> (a -> b) -> a -> IO ([Cycle], b)
ticksWHNF n0 f a = go f a n0 []
  where
    go f' a' n ts
      | n <= 0 = pure (reverse ts, f a)
      | otherwise = do
          (t,_) <- tickWHNFNoinline f a
          go f' a' (n - 1) (t:ts)
{-# NOINLINE ticksWHNF #-}

-- | WHNF version
ticksWHNFIO :: Int -> IO a -> IO ([Cycle], a)
ticksWHNFIO n0 a = go a n0 []
  where
    go a' n ts
      | n <= 0 = do
            a'' <- a'
            pure (reverse ts, a'')
      | otherwise = do
          (t,_) <- tickWHNFIONoinline a'
          go a' (n - 1) (t:ts)
{-# NOINLINE ticksWHNFIO #-}
