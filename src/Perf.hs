{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

-- | == Introduction
--
-- 'perf' provides high-resolution measurements of the runtime of Haskell functions. It does so by reading the RDTSC register (TSC stands for "time stamp counter"), which is present on all x86 CPUs since the Pentium architecture.
--
-- With 'perf' the user may measure both pure and effectful functions, as shown in the Example below. Every piece of code the user may want to profile is passed as an argument to the 'perf' function, along with a text label (that will be displayed in the final summary) and the measurement function (e.g. 'cycles', 'cputime' or 'realtime').
--
-- 'PerfT' is a monad transformer designed to collect performance information.
-- The transformer can be used to add performance measurent to existing code using 'Measure's.
--
--
-- Running the code produces a tuple of the original computation results, and a Map of performance measurements that were specified.  Indicative results:
--
-- == Note on RDTSC
--
-- Measuring program runtime with RDTSC comes with a set of caveats, such as portability issues, internal timer consistency in the case of multiprocessor architectures, and fluctuations due to power throttling. For more details, see : https://en.wikipedia.org/wiki/Time_Stamp_Counter
module Perf
  ( -- * re-exports
    module Perf.Algos,
    module Perf.Time,
    module Perf.BigO,
    module Perf.Space,
    module Perf.Report,
    module Perf.Stats,
    module Perf.Types,
    module Perf.Measure,
  )
where

import Prelude hiding (cycle)
import Perf.Algos
import Perf.Time
import Perf.BigO
import Perf.Space
import Perf.Report
import Perf.Stats
import Perf.Types
import Perf.Measure
