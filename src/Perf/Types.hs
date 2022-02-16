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
module Perf.Types
  ( Measure (..),
    repeated,
    StepMeasure (..),
    toMeasure,
    toMeasureN,
    step,
    stepM,
    multi,
    multiM,

    -- * applicants
    fap,
    fam,
    (|$|),
    ($|),

    -- * PerfT monad
    PerfT (..),
    Perf,
    runPerfT,
    evalPerfT,
    execPerfT,
  )
where

import Control.Monad.State.Lazy
import Data.Functor.Identity
import qualified Data.Map.Strict as Map
import Prelude
import Data.String
import Data.Text (Text)
import Data.Bifunctor

data StepMeasure m t = forall i. StepMeasure { pre :: m i, post :: i -> m t }

instance (Functor m) => Functor (StepMeasure m)
  where
    fmap f (StepMeasure start stop) = StepMeasure start (fmap f . stop)

instance (Applicative m) => Applicative (StepMeasure m)
  where
    pure t  = StepMeasure (pure ()) (const (pure t))
    (<*>) (StepMeasure fstart fstop) (StepMeasure start stop) =
      StepMeasure ((,) <$> fstart <*> start) (\(fi,i) -> fstop fi <*> stop i)

toMeasure :: (Monad m) => StepMeasure m t -> Measure m t
toMeasure (StepMeasure pre' post') = Measure (step pre' post') (stepM pre' post')
{-# INLINEABLE toMeasure #-}

toMeasureN :: (Monad m) => Int -> StepMeasure m t -> Measure m [t]
toMeasureN n (StepMeasure pre' post') = Measure (multi (step pre' post') n) (multiM (stepM pre' post') n)
{-# INLINEABLE toMeasureN #-}

-- | A single step measurement.
step :: Monad m => m i -> (i -> m t) -> (a -> b) -> a -> m (t, b)
step pre' post' !f !a = do
  !p <- pre'
  !b <- pure $! f a
  !t <- post' p
  pure (t, b)
{-# INLINEABLE step #-}

-- | A single step measurement.
stepM :: Monad m => m i -> (i -> m t) -> m a -> m (t, a)
stepM pre' post' a = do
  !p <- pre'
  !ma <- a
  !t <- post' p
  pure (t, ma)
{-# INLINEABLE stepM #-}

multi :: Monad m => ((a -> b) -> a -> m (t,b)) -> Int -> (a -> b) -> a -> m ([t], b)
multi action n !f !a =
  fmap (\xs -> (fmap fst xs, snd (head xs))) (replicateM n (action f a))
{-# INLINEABLE multi #-}


-- | Multiple measurements
multiM :: Monad m => (m a -> m (t,a)) -> Int -> m a -> m ([t], a)
multiM action n a =
  fmap (\xs -> (fmap fst xs, snd (head xs))) (replicateM n (action a))
{-# INLINEABLE multiM #-}

data Measure m t =
  Measure
  { measure :: forall a b. (a -> b) -> a -> m (t,b),
    measureM :: forall a. m a -> m (t,a)
  }

instance (Functor m) => Functor (Measure m) where
  fmap f (Measure m n) =
    Measure
    (\f' a' -> fmap (first f) (m f' a'))
    (fmap (first f) . n)

-- | An inefficient application that runs the inner action twice.
instance (Applicative m) => Applicative (Measure m) where
  pure t = Measure (\f a -> pure (t, f a)) (\a -> (t,) <$> a)
  (Measure mf nf) <*> (Measure mt nt) =
    Measure
    (\f a -> (\(nf',fa') (t',_) -> (nf' t',fa')) <$> mf f a <*> mt f a)
    (\a -> (\(nf',a') (t',_) -> (nf' t',a')) <$> nf a <*> nt a)

repeated :: (Applicative m) => Int -> Measure m t -> Measure m [t]
repeated n (Measure p m) =
  Measure
  (\f a -> fmap (\xs -> (fmap fst xs, snd (head xs))) (replicateM n (p f a)))
  (fmap (\xs -> (fmap fst xs, snd (head xs))) . replicateM n . m)
{-# INLINEABLE repeated #-}

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
fap :: (MonadIO m, Semigroup t) => Text -> (a -> b) -> a -> PerfT m t b
fap label f a =
  PerfT $ do
    m <- fst <$> get
    (t, fa) <- lift $ measure m f a
    modify $ second (Map.insertWith (<>) label t)
    return fa

-- | Lift a monadic value to a PerfT m, providing a label and a 'Measure'.
--
-- Measurements with the same label will be added
fam :: (MonadIO m, Semigroup t) => Text -> m a -> PerfT m t a
fam label a =
  PerfT $ do
    m <- fst <$> get
    (t, ma) <- lift $ measureM m a
    modify $ second (Map.insertWith (<>) label t)
    return ma

-- | lift a pure, unnamed function application to PerfT
(|$|) :: (Semigroup t) => (a -> b) -> a -> PerfT IO t b
(|$|) f a = fap "" f a

-- | lift a monadic, unnamed function application to PerfT
($|) :: (Semigroup t) => IO a -> PerfT IO t a
($|) a = fam "" a

-- | Run the performance measure, returning (computational result, measurement).
--
-- >>> (cs, result) <- runPerfT cycle' $ (foldl' (+) 0) |$| [0..10000]
--
-- > (50005000,fromList [("sum",562028)])
-- runPerfT :: Measure m t -> PerfT m t a -> Measure m t -> m (a, Map.Map Text t)
runPerfT :: (Functor m) => Measure m t -> PerfT m t a -> m (a, Map.Map Text t)
runPerfT m p = fmap (second snd) <$> flip runStateT (m, Map.empty) $ measurePerf p

-- | Consume the PerfT layer and return the original monadic result.
-- Fingers crossed, PerfT structure should be completely compiled away.
--
-- >>> result <- evalPerfT $ perf "sum" cycle' (foldl' (+) 0) [0..10000]
--
-- > 50005000
evalPerfT :: Monad m => Measure m t -> PerfT m t a -> m a
evalPerfT m p = fmap fst <$> flip runStateT (m, Map.empty) $ measurePerf p

-- | Consume a PerfT layer and return the measurement.
--
-- >>> cs <- execPerfT $ perf "sum" cycle' (foldl' (+) 0) [0..10000]
--
-- > fromList [("sum",562028)]
execPerfT :: Monad m => Measure m t -> PerfT m t a -> m (Map.Map Text t)
execPerfT m p = fmap snd <$> flip execStateT (m, Map.empty) $ measurePerf p

