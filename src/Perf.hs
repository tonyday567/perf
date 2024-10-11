-- | == Introduction
--
-- @perf@ provides tools for measuring the runtime performance of Haskell functions. It includes:
--
-- - time measurement via the [clock](https://hackage.haskell.org/package/clock) library.
--
-- - a polymorphic approach to what a 'Measure' is so that a wide variety of measurements such as counting, space and time measurement can share the same API.
--
-- - 'PerfT' which is a monad transformer designed to add the collection of performance information to existing code. Running the code produces a tuple of the original computation results, and a Map of performance measurements that were specified.
--
-- - functionality to determine performance order, in 'Perf.BigO'
--
-- - reporting functionality encapsulated in 'Perf.Report'. @perf@ can be run via 'cabal bench' and will, for example, error on performance degradation; see the project's cabal file for an example.
module Perf
  ( -- * re-exports
    module Perf.Types,
    -- | Representation of what a Performance 'Measure' is.
    module Perf.Measure,
    -- | Low-level time performance 'Measure' counting 'Nanos'
    module Perf.Time,
    -- | Low-level space performance 'Measure's based on GHC's allocation statistics.
    module Perf.Space,
    -- | Simple loop counter
    module Perf.Count,
    -- | Various (fast loop) algorithms that have been used for testing perf functionality.
    module Perf.Algos,
    -- | Order of complexity computations
    module Perf.BigO,
    -- | Reporting
    module Perf.Report,
    -- | Statistical support
    module Perf.Stats,
  )
where

import Perf.Algos
import Perf.BigO
import Perf.Count
import Perf.Measure
import Perf.Report
import Perf.Space
import Perf.Stats
import Perf.Time
import Perf.Types
import Prelude hiding (cycle)
