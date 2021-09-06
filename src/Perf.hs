{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}

-- | == Introduction
--
-- 'perf' provides high-resolution measurements of the runtime of Haskell functions. It does so by reading the RDTSC register (TSC stands for "time stamp counter"), which is present on all x86 CPUs since the Pentium architecture.
--
-- With 'perf' the user may measure both pure and effectful functions, as shown in the Example below. Every piece of code the user may want to profile is passed as an argument to the 'perf' function, along with a text label (that will be displayed in the final summary) and the measurement function (e.g. 'cycles', 'cputime' or 'realtime').
--
-- 'PerfT' is a monad transformer designed to collect performance information.
-- The transformer can be used to add performance measurent to existing code using 'Measure's.
--
-- == Example :
--
-- Code block to be profiled :
--
-- >   result <- do
-- >       txt <- readFile "examples/examples.hs"
-- >       let n = Text.length txt
-- >       let x = foldl' (+) 0 [1..n]
-- >       putStrLn $ "sum of one to number of characters is: " <>
-- >           (show x :: Text)
-- >       pure (n, x)
--
-- The same code, instrumented with 'perf' :
--
-- >   (result', ms) <- runPerfT $ do
-- >           txt <- perf "file read" cycles $ readFile "examples/examples.hs"
-- >           n <- perf "length" cycles $ pure (Text.length txt)
-- >           x <- perf "sum" cycles $ pure (foldl' (+) 0 [1..n])
-- >           perf "print to screen" cycles $
-- >               putStrLn $ "sum of one to number of characters is: " <>
-- >               (show x :: Text)
-- >           pure (n, x)
--
-- Running the code produces a tuple of the original computation results, and a Map of performance measurements that were specified.  Indicative results:
--
-- > file read                               4.92e5 cycles
-- > length                                  1.60e6 cycles
-- > print to screen                         1.06e5 cycles
-- > sum                                     8.12e3 cycles
--
-- == Note on RDTSC
--
-- Measuring program runtime with RDTSC comes with a set of caveats, such as portability issues, internal timer consistency in the case of multiprocessor architectures, and flucturations due to power throttling. For more details, see : https://en.wikipedia.org/wiki/Time_Stamp_Counter
module Perf
  ( PerfT,
    Perf,
    perf,
    perfN,
    runPerfT,
    evalPerfT,
    execPerfT,
    module Perf.Cycle,
    module Perf.Measure,
  )
where

import Control.Monad.IO.Class
import Control.Monad.State.Lazy
import Data.Functor.Identity
import qualified Data.Map as Map
import qualified Data.Text as T
import Perf.Cycle
import Perf.Measure
import Prelude

-- $setup
-- >>> import Perf.Cycle
-- >>> import Data.Foldable (foldl')

-- | PerfT is polymorphic in the type of measurement being performed.
-- The monad stores and produces a Map of labelled measurement values
newtype PerfT m b a = PerfT
  { runPerf_ :: StateT (Map.Map T.Text b) m a
  }
  deriving (Functor, Applicative, Monad)

-- | The obligatory transformer over Identity
type Perf b a = PerfT Identity b a

instance (MonadIO m) => MonadIO (PerfT m b) where
  liftIO = PerfT . liftIO

-- | Lift a monadic computation to a PerfT m, providing a label and a 'Measure'.
perf :: (MonadIO m, Num b) => T.Text -> Measure m b -> m a -> PerfT m b a
perf label m a =
  PerfT $ do
    st <- get
    (m', a') <- lift $ runMeasure m a
    put $ Map.insertWith (+) label m' st
    return a'

-- | Lift a monadic computation to a PerfT m, and carry out the computation multiple times.
perfN ::
  (MonadIO m, Monoid b) =>
  Int ->
  T.Text ->
  Measure m b ->
  m a ->
  PerfT m b a
perfN n label m a =
  PerfT $ do
    st <- get
    (m', a') <- lift $ runMeasureN n m a
    put $ Map.insertWith (<>) label m' st
    return a'

-- | Consume the PerfT layer and return a (result, measurement).
--
-- >>> :set -XOverloadedStrings
-- >>> (cs, result) <- runPerfT $ perf "sum" cycles (pure $ foldl' (+) 0 [0..10000])
--
-- > (50005000,fromList [("sum",562028)])
runPerfT :: PerfT m b a -> m (a, Map.Map T.Text b)
runPerfT p = flip runStateT Map.empty $ runPerf_ p

-- | Consume the PerfT layer and return the original monadic result.
-- Fingers crossed, PerfT structure should be completely compiled away.
--
-- >>> result <- evalPerfT $ perf "sum" cycles (pure $ foldl' (+) 0 [0..10000])
--
-- > 50005000
evalPerfT :: Monad m => PerfT m b a -> m a
evalPerfT p = flip evalStateT Map.empty $ runPerf_ p

-- | Consume a PerfT layer and return the measurement.
--
-- >>> cs <- execPerfT $ perf "sum" cycles (pure $ foldl' (+) 0 [0..10000])
--
-- > fromList [("sum",562028)]
execPerfT :: Monad m => PerfT m b a -> m (Map.Map T.Text b)
execPerfT p = flip execStateT Map.empty $ runPerf_ p
