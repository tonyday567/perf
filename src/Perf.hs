{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Perf
    (
    -- * The Perf Monad
      PerfT
    , Perf
    , perf
    , perfN
    , runPerfT
    , evalPerfT
    , execPerfT
    , module Perf.Cycles
    , module Perf.Measure
    )
    where

import Protolude
import Perf.Measure
import Perf.Cycles
import qualified Data.Map as Map

newtype PerfT m b a =
  PerfT { runPerf_ :: StateT (Map.Map Text b) m a }
  deriving
  ( Functor
  , Applicative
  , Monad
  )

type Perf b a = PerfT Identity b a

instance (MonadIO m) => MonadIO (PerfT m b) where
  liftIO = PerfT . liftIO

perf :: (MonadIO m, Monoid b, Semigroup b) => Text -> Measure m b -> m a -> PerfT m b a
perf label m a = PerfT $ do
  st <- get
  (m', a') <- lift $ runMeasure m a
  put $ Map.insertWith (<>) label m' st
  return a'

perfN :: (MonadIO m, Semigroup b, Monoid b) => Int -> Text -> Measure m b -> m a -> PerfT m b a
perfN n label m a = PerfT $ do
  st <- get
  (m', a') <- lift $ runMeasureN n m a
  put $ Map.insertWith (<>) label m' st
  return a'

runPerfT :: PerfT m b a -> m (a, Map.Map Text b)
runPerfT p =
  flip runStateT Map.empty $ runPerf_ p

evalPerfT :: (Monad m) => PerfT m b a -> m a
evalPerfT p =
  flip evalStateT Map.empty $ runPerf_ p

execPerfT :: (Monad m) => PerfT m b a -> m (Map.Map Text b)
execPerfT p =
  flip execStateT Map.empty $ runPerf_ p
