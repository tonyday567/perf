{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

-- | Space performance measurement.
module Perf.Space
  ( SpaceStats (..),
    ssToList,
    spaceLabels,
    space,
    allocation,
    Bytes (..),
  )
where

import Data.Text (Text)
import Data.Word
import GHC.Stats
import Perf.Types
import System.Mem
import Prelude hiding (cycle)
import Control.Monad

-- | GHC allocation statistics.
data SpaceStats = SpaceStats {allocatedBytes :: Word64, gcollects :: Word32, maxLiveBytes :: Word64, gcLiveBytes :: Word64, maxMem :: Word64} deriving (Read, Show, Eq)

-- | Convert 'SpaceStats' to a list of numbers.
ssToList :: (Num a) => SpaceStats -> [a]
ssToList (SpaceStats x1 x2 x3 x4 x5) = [fromIntegral x1, fromIntegral x2, fromIntegral x3, fromIntegral x4, fromIntegral x5]

instance Semigroup SpaceStats where
  (<>) = addSpace

instance Monoid SpaceStats where
  mempty = SpaceStats 0 0 0 0 0

instance Num SpaceStats where
  (+) = addSpace
  (-) = diffSpace
  (*) = error "SpaceStats times"
  abs = error "SpaceStats abs"
  signum = error "SpaceStats signum"
  fromInteger n = SpaceStats (fromIntegral n) (fromIntegral n) (fromIntegral n) (fromIntegral n) (fromIntegral n)

diffSpace :: SpaceStats -> SpaceStats -> SpaceStats
diffSpace (SpaceStats x1 x2 x3 x4 x5) (SpaceStats x1' x2' x3' x4' x5') = SpaceStats (x1' - x1) (x2' - x2) (x3' - x3) (x4' - x4) (x5' - x5)

addSpace :: SpaceStats -> SpaceStats -> SpaceStats
addSpace (SpaceStats x1 x2 x3 x4 x5) (SpaceStats x1' x2' x3' x4' x5') = SpaceStats (x1' + x1) (x2' + x2) (x3' + x3) (x4' + x4) (x5' + x5)

getSpace :: RTSStats -> SpaceStats
getSpace s = SpaceStats (allocated_bytes s) (gcs s) (max_live_bytes s) (gcdetails_live_bytes (gc s)) (max_mem_in_use_bytes s)

-- | Labels for 'SpaceStats'.
spaceLabels :: [Text]
spaceLabels = ["allocated", "gcollects", "maxLiveBytes", "gcLiveBytes", "MaxMem"]

-- | A allocation 'StepMeasure' with a flag to determine if 'performGC' should run prior to the measurement.
space :: Bool -> StepMeasure IO SpaceStats
space p = StepMeasure (start p) stop
  where
    start p' = do
      when p' performGC
      getSpace <$> getRTSStats
    stop s = do
      s' <- getSpace <$> getRTSStats
      pure $ diffSpace s s'
{-# INLINEABLE space #-}

-- | Number of bytes
newtype Bytes = Bytes {unbytes :: Word64}
  deriving (Show, Read, Eq, Ord, Num, Real, Enum, Integral)

instance Semigroup Bytes where
  (<>) = (+)

instance Monoid Bytes where
  mempty = 0

-- | Measure memory allocation, with a flag to run 'performGC' prior to the measurement.
allocation :: Bool -> StepMeasure IO Bytes
allocation p = StepMeasure (start p) stop
  where
    start p' = do
      when p' performGC
      Bytes . allocated_bytes <$> getRTSStats
    stop s = do
      s' <- Bytes . allocated_bytes <$> getRTSStats
      pure $ s' - s
{-# INLINEABLE allocation #-}
