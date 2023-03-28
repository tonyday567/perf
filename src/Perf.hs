{-# OPTIONS_GHC -Wall #-}

-- | == Introduction
--
-- /perf/ provides tools for measuring the runtime performance of Haskell functions. It includes:
--
-- - time measurement via reading the RDTSC register (TSC stands for "time stamp counter"), which is present on all x86 CPUs since the Pentium architecture. For more details, see  https://en.wikipedia.org/wiki/Time_Stamp_Counter
--
-- - abstraction of what is a 'Measure' so that the library includes both space and time measurement with the same API.
--
-- - 'PerfT' which is a monad transformer designed to add the collection of performance information to existing code. Running the code produces a tuple of the original computation results, and a Map of performance measurements that were specified.
--
-- - functionality to determine performance order, in 'Perf.BigO'
--
-- Usage examples can be found in app/perf-explore.hs and the project's readme.org.
module Perf
  ( -- * re-exports

    -- | Various (fast loop) algorithms that have been used for testing perf functionality.
    module Perf.Algos,
    -- | Low-level time performance 'Measure's counting 'Cycles'
    module Perf.Time,
    -- | Order of complexity computations
    module Perf.BigO,
    -- | Low-level space performance 'Measure's based on GHC's allocation statistics.
    module Perf.Space,
    -- | Reporting, including 'Golden' file functionality.
    module Perf.Report,
    module Perf.Stats,
    module Perf.Types,
    module Perf.Measure,
  )
where

import Perf.Algos
import Perf.BigO
import Perf.Measure
import Perf.Report
import Perf.Space
import Perf.Stats
import Perf.Time
import Perf.Types
import Prelude hiding (cycle)
