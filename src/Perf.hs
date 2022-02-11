{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

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
  ( Measure (..),
    measure,
    measureM,

    -- * PerfT monad
    perf,
    perfM,
    PerfT,
    Perf,
    runPerfT,
    evalPerfT,
    execPerfT,

    -- * specific measures
    module Perf.Cycle,
    cost,
    cputime,
    realtime,
    cycles,
    count,

    -- * example
    example1,
    example1',

    -- * operators
    (|$|),
    (~$~),
  )
where

import Control.Monad.State.Lazy
import Data.Functor.Identity
import qualified Data.Map as Map
import qualified Data.Text.IO as Text
import Perf.Cycle
import Prelude
import Data.Time
import Data.Fixed
import System.CPUTime
import Data.String
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Foldable
import Data.Bifunctor

-- $setup
-- >>> import Perf.Cycle
-- >>> import Data.Foldable (foldl')

-- | A Measure consists of a monadic effect prior to measuring, a monadic effect to finalise the measurement, and the value measured
--
-- For example, the measure specified below will return 1 every time measurement is requested, thus forming the base of a simple counter for loopy code.
--
-- >>> let count = Measure 0 (pure ()) (pure 1)
data Measure m t = forall i.
  Measure
  { prestep :: m i,
    poststep :: i -> m t
  }

-- | Measure the performance of a pure computation.
measure :: Monad m => Measure m t -> (a -> b) -> a -> m (t, b)
measure (Measure pre post) !f !a = do
  !p <- pre
  !b <- pure $! f a
  !t <- post p
  pure (t, b)

-- | Measure the performance of a monadic computation.
measureM :: Monad m => Measure m t -> (a -> m b) -> a -> m (t, b)
measureM (Measure pre post) !f !a = do
  !p <- pre
  !b <- f a
  !t <- post p
  pure (t, b)

-- | Performance measurement transformer
newtype PerfT m t a = PerfT
  { measurePerf :: StateT (Measure m t, Map.Map Text t) m a
  }
  deriving (Functor, Applicative, Monad)

-- | The obligatory transformer over Identity
type Perf t a = PerfT Identity t a

instance (MonadIO m) => MonadIO (PerfT m t) where
  liftIO = PerfT . liftIO

-- | Lift an application to a PerfT m, providing a label and a 'Measure'.
--
-- Measurements with the same label will be added
perf :: (MonadIO m, Num t) => Text -> (a -> b) -> a -> PerfT m t b
perf label f a =
  PerfT $ do
    m <- fst <$> get
    (t, fa) <- lift $ measure m f a
    modify $ second (Map.insertWith (+) label t)
    return fa

-- | Lift an application to a PerfT m, providing a label and a 'Measure'.
--
-- Measurements with the same label will be added
perfM :: (MonadIO m, Num t) => Text -> (a -> m b) -> a -> PerfT m t b
perfM label f a =
  PerfT $ do
    m <- fst <$> get
    (t, fa) <- lift $ measureM m f a
    modify $ second (Map.insertWith (+) label t)
    return fa

-- | lift a pure function application to PerfT
--
-- >>> fap "sum" (foldl' (+) 0) [0..10000]
fap :: Text -> (a -> b) -> a -> PerfT IO Cycle b
fap label f a = perf label f a

-- | lift a monadic function application to PerfT
--
-- >>> fam "sum" (pure . foldl' (+) 0) [0..10000]
fam :: Text -> (a -> IO b) -> a -> PerfT IO Cycle b
fam label f a = perfM label f a

-- | lift a pure, unnamed function application to PerfT
(|$|) :: (a -> b) -> a -> PerfT IO Cycle b
(|$|) f a = fap "" f a

-- | lift a monadic, unnamed function application to PerfT
(~$~) :: (a -> IO b) -> a -> PerfT IO Cycle b
(~$~) f a = fam "" f a

-- | Run the performance measure, returning (computational result, measurement).
--
-- >>> (cs, result) <- runPerfT cycles $ (foldl' (+) 0) |$| [0..10000]
--
-- > (50005000,fromList [("sum",562028)])
-- runPerfT :: Measure m t -> PerfT m t a -> Measure m t -> m (a, Map.Map Text t)
runPerfT :: (Functor m) => Measure m t -> PerfT m t a -> m (a, Map.Map Text t)
runPerfT m p = fmap (second snd) <$> flip runStateT (m, Map.empty) $ measurePerf p

-- | Consume the PerfT layer and return the original monadic result.
-- Fingers crossed, PerfT structure should be completely compiled away.
--
-- >>> result <- evalPerfT $ perf "sum" cycles (foldl' (+) 0) [0..10000]
--
-- > 50005000
evalPerfT :: Monad m => Measure m t -> PerfT m t a -> m a
evalPerfT m p = fmap fst <$> flip runStateT (m, Map.empty) $ measurePerf p

-- | Consume a PerfT layer and return the measurement.
--
-- >>> cs <- execPerfT $ perf "sum" cycles (foldl' (+) 0) [0..10000]
--
-- > fromList [("sum",562028)]
execPerfT :: Monad m => Measure m t -> PerfT m t a -> m (Map.Map Text t)
execPerfT m p = fmap snd <$> flip execStateT (m, Map.empty) $ measurePerf p

-- | cost of a measurement in terms of the Measure's own units
--
-- >>> r <- cost count
-- >>> r
-- 1
cost :: Monad m => Measure m b -> m b
cost (Measure pre post) = pre >>= post

-- | a measure using 'getCPUTime' from System.CPUTime (unit is picoseconds)
--
-- >>> r <- measure cputime (foldl' (+) 0) [0..1000]
--
-- > (34000000,500500)
cputime :: Measure IO Integer
cputime = Measure start stop
  where
    start = getCPUTime
    stop a = do
      t <- getCPUTime
      return $ t - a

-- | a measure using 'getCurrentTime' (unit is seconds)
--
-- >>> r <- measure realtime (foldl' (+) 0) [0..1000]
--
-- > (0.000046,500500)
realtime :: Measure IO Double
realtime = Measure start stop
  where
    start = getCurrentTime
    stop a = do
      t <- getCurrentTime
      return $ fromNominalDiffTime $ diffUTCTime t a

fromNominalDiffTime :: NominalDiffTime -> Double
fromNominalDiffTime t = fromInteger i * 1e-12
  where
    (MkFixed i) = nominalDiffTimeToSeconds t

-- | a 'Measure' used to count iterations
--
-- >>> r <- measure count (pure ())
-- >>> r
-- (1,())
count :: Measure IO Int
count = Measure start stop
  where
    start = return ()
    stop () = return 1

-- | a 'Measure' using the 'rdtsc' CPU register (units are in cycles)
--
-- >>> r <- measure cycles (const ()) ()
--
-- > (120540,()) -- ghci-level
-- > (18673,())  -- compiled with -O2
cycles :: Measure IO Cycle
cycles = Measure start stop
  where
    start = rdtsc
    stop a = do
      t <- rdtsc
      return $ t - a

example1 :: IO Int
example1 = do
     txt <- Text.readFile "src/Perf.hs"
     let n = Text.length txt
     let x = foldl' (+) 0 [1..n]
     Text.putStrLn $ "sum of one to number of characters is: " <> Text.pack (show x)
     pure x

example1' :: IO (Int, Map.Map Text Cycle)
example1' = runPerfT cycles $ do
     txt <- Text.readFile ~$~ "src/Perf.hs"
     let n = Text.length txt
     f <- foldl' (+) 0 |$| [1..n]
     (Text.putStrLn .
       ("sum of one to number of characters is: " <>) .
       Text.pack .
       show) ~$~ f
     pure f

-- >   (result', ms) <- runPerfT $ do
-- >           txt <- perf "file read" cycles $ readFile "examples/examples.hs"
-- >           n <- perf "length" cycles $ pure (Text.length txt)
-- >           x <- perf "sum" cycles $ pure (foldl' (+) 0 [1..n])
-- >           perf "print to screen" cycles $
-- >               putStrLn $ "sum of one to number of characters is: " <>
-- >               (show x :: Text)
-- >           pure (n, x)
