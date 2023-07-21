{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Simple counter.
module Perf.Count
  ( count,
    countN,
  )
where

import Perf.Types
import Prelude

-- | Register 1 as a performance measure
count :: (Applicative m) => StepMeasure m Int
count = StepMeasure start stop
  where
    start = pure ()
    stop _ = pure 1
{-# INLINEABLE count #-}

-- | Count the number of times measured.
countN :: Int -> Measure IO Int
countN n = fmap sum $ toMeasureN n $ count
{-# INLINEABLE countN #-}
