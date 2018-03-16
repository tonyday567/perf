{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Specification of a performance measurement type suitable for the 'PerfT' monad transformer.
module Perf.Measure
  ( Measure(..)
  , runMeasure
  , runMeasureN
  , cost
  , cputime
  , realtime
  , count
  , cycles
  , Additive(..)
  )
  where

import Control.Monad.IO.Class (MonadIO(..))

import Data.Time.Clock
import GHC.Word (Word64)
import Control.Monad (replicateM_)
import Data.Foldable (foldl')
import Perf.Cycle 
import System.CPUTime
import System.CPUTime.Rdtsc


-- | Lightweight 'Additive' class. 
class Num a => Additive a where
  add :: a -> a -> a
  zero :: a

instance Additive Int where
  add = (+)
  zero = 0

instance Additive Integer where
  add = (+)
  zero = 0

instance Additive Word64 where
  add = (+)
  zero = 0

instance Additive NominalDiffTime where
  add = (+)
  zero = 0
  


-- | A Measure consists of a monadic effect prior to measuring, a monadic effect to finalise the measurement, and the value measured
--
-- For example, the measure specified below will return 1 every time measurement is requested, thus forming the base of a simple counter for loopy code.
--
-- >>> let count = Measure 0 (pure ()) (pure 1)
data Measure m b = forall a. (Additive b) => Measure
  { measure :: b
  , prestep :: m a
  , poststep :: a -> m b
  }


-- | Measure a single effect.
--
-- >>> r <- runMeasure count (pure "joy")
-- >>> r
-- (1,"joy")
--
runMeasure :: Monad m => Measure m b -> m a -> m (b, a)
runMeasure (Measure _ pre post) a = do
  p <- pre
  !a' <- a
  m' <- post p
  return (m', a')

-- | Measure once, but run an effect multiple times.
--
-- >>> r <- runMeasureN 1000 count (pure "joys")
-- >>> r
-- (1,"joys")
--
runMeasureN :: Monad m => Int -> Measure m b -> m a -> m (b, a)
runMeasureN n (Measure _ pre post) a = do
  p <- pre
  replicateM_ (n - 1) a
  !a' <- a
  m' <- post p
  return (m', a')

-- | cost of a measurement in terms of the Measure's own units
--
-- >>> r <- cost count
-- >>> r
-- 1
cost :: Monad m => Measure m b -> m b
cost (Measure _ pre post) = pre >>= post 

-- | a measure using 'getCPUTime' from System.CPUTime (unit is picoseconds)
--
-- >>> r <- runMeasure cputime (pure $ foldl' (+) 0 [0..1000])
--
-- > (34000000,500500)
--
cputime :: Measure IO Integer
cputime = Measure 0 start stop
  where
    start = getCPUTime
    stop a = do
      t <- getCPUTime
      return $ t - a



-- | a measure using 'getCurrentTime' (unit is 'NominalDiffTime' which prints as seconds)
--
-- >>> r <- runMeasure realtime (pure $ foldl' (+) 0 [0..1000])
--
-- > (0.000046s,500500)
--
realtime :: Measure IO NominalDiffTime
realtime = Measure m0 start stop
  where
    m0 = zero :: NominalDiffTime
    start = getCurrentTime
    stop a = do
      t <- getCurrentTime
      return $ diffUTCTime t a

-- | a 'Measure' used to count iterations
--
-- >>> r <- runMeasure count (pure ())
-- >>> r
-- (1,())
--
count :: Measure IO Int
count = Measure m0 start stop
  where
    m0 = 0 :: Int
    start = return ()
    stop () = return 1

-- | a 'Measure' using the 'rdtsc' CPU register (units are in cycles)
--
-- >>> r <- runMeasureN 1000 cycles (pure ())
-- 
-- > (120540,()) -- ghci-level
-- > (18673,())  -- compiled with -O2
--
cycles :: Measure IO Cycle
cycles = Measure m0 start stop
  where
    m0 = 0
    start = rdtsc
    stop a = do
      t <- rdtsc
      return $ t - a

