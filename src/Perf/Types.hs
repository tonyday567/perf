{-# LANGUAGE OverloadedStrings #-}

-- | Abstract types of performance measurement.
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

    -- * function application
    fap,
    afap,
    ffap,
    fan,
    fam,
    (|$|),
    ($|),
    (|+|),

    -- * PerfT monad
    PerfT (..),
    Perf,
    runPerfT,
    evalPerfT,
    execPerfT,
    outer,
    slop,
    slops,
  )
where

import Control.DeepSeq
import Control.Monad
import Control.Monad.State.Lazy
import Data.Bifunctor
import Data.Functor.Identity
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Prelude

-- | Abstraction of a performance measurement within a monadic context.
--
-- - measure applies a function to a value, returning a tuple of the performance measure, and the computation result.
-- - measureM evaluates a monadic value and returns a performance-result tuple.
data Measure m t = Measure
  { measure :: forall a b. (a -> b) -> a -> m (t, b),
    measureM :: forall a. m a -> m (t, a)
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
      (\f a -> (\(nf', fa') (t', _) -> (nf' t', fa')) <$> mf f a <*> mt f a)
      (\a -> (\(nf', a') (t', _) -> (nf' t', a')) <$> nf a <*> nt a)

-- | Convert a Measure into a multi measure.
repeated :: (Applicative m) => Int -> Measure m t -> Measure m [t]
repeated n (Measure p m) =
  Measure
    (\f a -> fmap (\xs -> (fmap fst xs, snd (head xs))) (replicateM n (p f a)))
    (fmap (\xs -> (fmap fst xs, snd (head xs))) . replicateM n . m)
{-# INLINEABLE repeated #-}

-- | Abstraction of a performance measurement with a pre and a post step wrapping the computation.
data StepMeasure m t = forall i. StepMeasure {pre :: m i, post :: i -> m t}

instance (Functor m) => Functor (StepMeasure m) where
  fmap f (StepMeasure start stop) = StepMeasure start (fmap f . stop)

instance (Applicative m) => Applicative (StepMeasure m) where
  pure t = StepMeasure (pure ()) (const (pure t))
  (<*>) (StepMeasure fstart fstop) (StepMeasure start stop) =
    StepMeasure ((,) <$> fstart <*> start) (\(fi, i) -> fstop fi <*> stop i)

-- | Convert a StepMeasure into a Measure
toMeasure :: (Monad m) => StepMeasure m t -> Measure m t
toMeasure (StepMeasure pre' post') = Measure (step pre' post') (stepM pre' post')
{-# INLINEABLE toMeasure #-}

-- | Convert a StepMeasure into a Measure running the computation multiple times.
toMeasureN :: (Monad m) => Int -> StepMeasure m t -> Measure m [t]
toMeasureN n (StepMeasure pre' post') = Measure (multi (step pre' post') n) (multiM (stepM pre' post') n)
{-# INLINEABLE toMeasureN #-}

-- | A single step measurement.
step :: (Monad m) => m i -> (i -> m t) -> (a -> b) -> a -> m (t, b)
step pre' post' !f !a = do
  !p <- pre'
  !b <- pure $! f a
  !t <- post' p
  pure (t, b)
{-# INLINEABLE step #-}

-- | A single step measurement.
stepM :: (Monad m) => m i -> (i -> m t) -> m a -> m (t, a)
stepM pre' post' a = do
  !p <- pre'
  !ma <- a
  !t <- post' p
  pure (t, ma)
{-# INLINEABLE stepM #-}

-- | Multiple measurement
multi :: (Monad m) => ((a -> b) -> a -> m (t, b)) -> Int -> (a -> b) -> a -> m ([t], b)
multi action n !f !a =
  fmap (\xs -> (fmap fst xs, snd (head xs))) (replicateM n (action f a))
{-# INLINEABLE multi #-}

-- | Multiple measurements
multiM :: (Monad m) => (m a -> m (t, a)) -> Int -> m a -> m ([t], a)
multiM action n a =
  fmap (\xs -> (fmap fst xs, snd (head xs))) (replicateM n (action a))
{-# INLINEABLE multiM #-}

-- | Performance measurement transformer storing a 'Measure' and a map of named results.
newtype PerfT m t a = PerfT
  { measurePerf :: StateT (Measure m t, Map.Map Text t) m a
  }
  deriving (Functor, Applicative, Monad)

-- | The transformer over Identity
type Perf t a = PerfT Identity t a

instance (MonadIO m) => MonadIO (PerfT m t) where
  liftIO = PerfT . liftIO

-- | Lift an application to a PerfT m, providing a label and a 'Measure'.
--
-- Measurements with the same label will be mappended
fap :: (MonadIO m, Semigroup t) => Text -> (a -> b) -> a -> PerfT m t b
fap label f a =
  PerfT $ do
    m <- fst <$> get
    (t, fa) <- lift $ measure m f a
    modify $ second (Map.insertWith (<>) label t)
    return fa
{-# INLINEABLE fap #-}

-- | Lift an application to a PerfT m, forcing the argument.
afap :: (NFData a, MonadIO m, Semigroup t) => Text -> (a -> b) -> a -> PerfT m t b
afap label f a = fap label f (force a)
{-# INLINEABLE afap #-}

-- | Lift an application to a PerfT m, forcing argument and result.
ffap :: (NFData a, NFData b, MonadIO m, Semigroup t) => Text -> (a -> b) -> a -> PerfT m t b
ffap label f a = fap label (force . f) (force a)
{-# INLINEABLE ffap #-}

-- | Lift a number to a PerfT m, providing a label, function, and input.
--
-- Measurements with the same label will be added
fan :: (MonadIO m, Num t) => Text -> (a -> b) -> a -> PerfT m t b
fan label f a =
  PerfT $ do
    m <- fst <$> get
    (t, fa) <- lift $ measure m f a
    modify $ second (Map.insertWith (+) label t)
    return fa
{-# INLINEABLE fan #-}

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
{-# INLINEABLE fam #-}

-- | lift a pure, unnamed function application to PerfT
(|$|) :: (Semigroup t) => (a -> b) -> a -> PerfT IO t b
(|$|) f a = fap "" f a
{-# INLINEABLE (|$|) #-}

-- | lift a monadic, unnamed function application to PerfT
($|) :: (Semigroup t) => IO a -> PerfT IO t a
($|) a = fam "" a
{-# INLINEABLE ($|) #-}

-- | lift an unnamed numeric measure to PerfT
(|+|) :: (Num t) => (a -> b) -> a -> PerfT IO t b
(|+|) f a = fan "" f a
{-# INLINEABLE (|+|) #-}

-- | Run the performance measure, returning (computational result, measurement).
runPerfT :: (Functor m) => Measure m t -> PerfT m t a -> m (a, Map.Map Text t)
runPerfT m p = fmap (second snd) <$> flip runStateT (m, Map.empty) $ measurePerf p
{-# INLINEABLE runPerfT #-}

-- | Consume the PerfT layer and return the original monadic result.
-- Fingers crossed, PerfT structure should be completely compiled away.
evalPerfT :: (Monad m) => Measure m t -> PerfT m t a -> m a
evalPerfT m p = fmap fst <$> flip runStateT (m, Map.empty) $ measurePerf p
{-# INLINEABLE evalPerfT #-}

-- | Consume a PerfT layer and return the measurement.
execPerfT :: (Monad m) => Measure m t -> PerfT m t a -> m (Map.Map Text t)
execPerfT m p = fmap snd <$> flip execStateT (m, Map.empty) $ measurePerf p
{-# INLINEABLE execPerfT #-}

-- | run a PerfT and also calculate performance over the entire computation
outer :: (MonadIO m, Semigroup s) => Text -> Measure m s -> Measure m t -> PerfT m t a -> m (a, (Map.Map Text s, Map.Map Text t))
outer label outerm meas p =
  (\((a, m), m') -> (a, (m', m)))
    <$> runPerfT
      outerm
      ( fam label (runPerfT meas p)
      )

-- | run a PerfT and calculate excess performance over the entire computation
slop :: (MonadIO m, Num t, Semigroup t) => Text -> Measure m t -> PerfT m t a -> m (a, Map.Map Text t)
slop l meas p =
  (\((a, m), m') -> (a, m <> Map.insert "slop" (m' Map.! l - Map.foldl' (+) 0 m) m'))
    <$> runPerfT
      meas
      ( fam l (runPerfT meas p)
      )

-- | run a multi PerfT and calculate excess performance over the entire computation
slops :: (MonadIO m, Num t, Semigroup t) => Int -> Measure m t -> PerfT m [t] a -> m (a, (Map.Map Text t, Map.Map Text [t]))
slops n meas p =
  (\((a, ms), m') -> (a, (Map.insert "slop" (m' Map.! "outer" - Map.foldl' (+) 0 (fmap sum ms)) m', ms)))
    <$> runPerfT
      meas
      ( fam "outer" (runPerfT (repeated n meas) p)
      )
