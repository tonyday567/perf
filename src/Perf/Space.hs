{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Perf.Space
  (
    SpaceStats (..),
    spaceLabels,
    space,
    allocated,
  )
where

import Perf.Types
import Control.Monad.State.Lazy
import Prelude hiding (cycle)
import Data.String
import GHC.Stats
import Data.Word
import System.Mem
import Data.Text (Text)

data SpaceStats = SpaceStats { allocatedBytes :: Word64, gcollects :: Word32, maxLiveBytes :: Word64, gcLiveBytes :: Word64, maxMem :: Word64 } deriving (Read, Show, Eq)

instance Semigroup SpaceStats
  where
    (<>) = addSpace

instance Monoid SpaceStats
  where
    mempty = SpaceStats 0 0 0 0 0

instance Num SpaceStats
  where
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

spaceLabels :: [Text]
spaceLabels = ["allocated","gcollects","maxLiveBytes","gcLiveBytes","MaxMem"]

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

newtype Bytes = Bytes { unbytes :: Word64 }
  deriving (Show, Read, Eq, Ord, Num, Real, Enum, Integral)

instance Semigroup Bytes where
  (<>) = (+)

instance Monoid Bytes where
  mempty = 0

allocated :: Bool -> StepMeasure IO Bytes
allocated p = StepMeasure (start p) stop
  where
    start p' = do
      when p' performGC
      Bytes . allocated_bytes <$> getRTSStats
    stop s = do
      s' <- Bytes . allocated_bytes <$> getRTSStats
      pure $ s' - s
{-# INLINEABLE allocated #-}
