{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Perf.Measure
    where

import Protolude

import Data.Time.Clock
import System.CPUTime
import Perf.Cycles as C
import System.CPUTime.Rdtsc

data Measure m b = forall a. (Monoid b) => Measure
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

-- instances
instance Monoid Integer where
  mempty = 0
  mappend = (+)

cputime :: Measure IO Integer
cputime = Measure 0 start stop
  where
    start = getCPUTime
    stop a = do
      t <- getCPUTime
      return $ t - a

instance Monoid NominalDiffTime where
  mempty = 0
  mappend = (+)

realtime :: Measure IO NominalDiffTime
realtime = Measure m0 start stop
  where
    m0 = fromInteger (0::Integer) :: NominalDiffTime
    start = getCurrentTime
    stop a = do
      t <- getCurrentTime
      return $ diffUTCTime t a

instance Monoid Int where
  mempty = 0
  mappend = (+)

count :: Measure IO Int
count = Measure m0 start stop
  where
    m0 = 0::Int
    start = return ()
    stop () = return 1

cycles :: Measure IO Cycles
cycles = Measure m0 start stop
  where
    m0 = 0
    start = rdtsc
    stop a = do
      t <- rdtsc
      return $ t - a


