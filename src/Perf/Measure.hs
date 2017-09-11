{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Perf.Measure where

import Data.Time.Clock
import NumHask.Prelude
import Perf.Cycle as C
import qualified Protolude as P
import System.CPUTime
import System.CPUTime.Rdtsc

data Measure m b = forall a. (Additive b) => Measure
  { measure :: b
  , prestep :: m a
  , poststep :: a -> m b
  }

runMeasure :: (MonadIO m) => Measure m b -> m a -> m (b, a)
runMeasure (Measure _ pre post) a = do
  p <- pre
  !a' <- a
  m' <- post p
  return (m', a')

runMeasureN :: (MonadIO m) => Int -> Measure m b -> m a -> m (b, a)
runMeasureN n (Measure _ pre post) a = do
  p <- pre
  replicateM_ (n - 1) a
  !a' <- a
  m' <- post p
  return (m', a')

cost :: (MonadIO m) => Measure m b -> m b
cost (Measure _ pre post) = do
  p <- pre
  post p

cputime :: Measure IO Integer
cputime = Measure 0 start stop
  where
    start = getCPUTime
    stop a = do
      t <- getCPUTime
      return $ t - a

instance AdditiveMagma NominalDiffTime where
  plus = (P.+)

instance AdditiveUnital NominalDiffTime where
  zero = 0

instance AdditiveAssociative NominalDiffTime

instance AdditiveCommutative NominalDiffTime

instance Additive NominalDiffTime

instance AdditiveInvertible NominalDiffTime where
  negate = P.negate

instance AdditiveGroup NominalDiffTime

realtime :: Measure IO NominalDiffTime
realtime = Measure m0 start stop
  where
    m0 = zero :: NominalDiffTime
    start = getCurrentTime
    stop a = do
      t <- getCurrentTime
      return $ diffUTCTime t a

count :: Measure IO Int
count = Measure m0 start stop
  where
    m0 = 0 :: Int
    start = return ()
    stop () = return 1

cycles :: Measure IO Cycle
cycles = Measure m0 start stop
  where
    m0 = 0
    start = rdtsc
    stop a = do
      t <- rdtsc
      return $ t - a
