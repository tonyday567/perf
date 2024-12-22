{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

-- | Abstract types of performance measurement.
module Perf.Types
  ( -- * Measure
    Measure (..),
    repeated,
    StepMeasure (..),
    toMeasure,
    toMeasureN,
    step,
    stepM,
    multi,
    multiM,
    multiN,

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
import GHC.Exts
import GHC.IO hiding (liftIO)
import Prelude

-- | Abstraction of a performance measurement within a monadic context.
--
-- - measure applies a function to a value, returning a tuple of the performance measure, and the computation result.
-- - measureM evaluates a monadic value and returns a performance-result tuple.
newtype Measure m t = Measure
  { measure :: forall a b. (a -> b) -> a -> m (t, b)
  }

instance (Functor m) => Functor (Measure m) where
  fmap f (Measure m) =
    Measure
      (\f' a' -> fmap (first f) (m f' a'))

-- | An inefficient application that runs the inner action twice.
instance (Applicative m) => Applicative (Measure m) where
  pure t = Measure (\f a -> pure (t, f a))
  (Measure mf) <*> (Measure mt) =
    Measure
      (\f a -> (\(nf', fa') (t', _) -> (nf' t', fa')) <$> mf f a <*> mt f a)

-- | Convert a Measure into a multi measure.
repeated :: (Applicative m) => Int -> Measure m t -> Measure m [t]
repeated n (Measure p) =
  Measure
    (\f a -> fmap (\xs -> (fmap fst xs, snd (head xs))) (replicateM n (p f a)))
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
toMeasure (StepMeasure pre' post') = Measure (step pre' post')
{-# INLINEABLE toMeasure #-}

-- | Convert a StepMeasure into a Measure running the computation multiple times.
toMeasureN :: (Monad m) => Int -> StepMeasure m t -> Measure m [t]
toMeasureN n (StepMeasure pre' post') = Measure (multi (step pre' post') n)
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

multi1 :: (Monad m) => ((a -> b) -> a -> m (t, b)) -> Int -> (a -> b) -> a -> m [(t, b)]
multi1 action n !f !a = sequence $ replicate n $! action f a
{-# INLINEABLE multi1 #-}

-- | Return one result but multiple measurements.
multi :: (Monad m) => ((a -> b) -> a -> m (t, b)) -> Int -> (a -> b) -> a -> m ([t], b)
multi action n !f !a = do
  xs <- multi1 action n f a
  pure (fmap fst xs, snd (head xs))
{-# INLINEABLE multi #-}

-- | Multiple measurements
multiM :: (Monad m) => (m a -> m (t, a)) -> Int -> m a -> m ([t], a)
multiM action n a =
  fmap (\xs -> (fmap fst xs, head $! fmap snd xs)) (replicateM n (action a))
{-# INLINEABLE multiM #-}

multiN :: (b -> t) -> (a -> b) -> a -> Int -> IO t
multiN frc = multiNLoop SPEC
  where
    -- Here we rely on the fact that GHC (unless spurred by
    -- -fstatic-argument-transformation) is not smart enough:
    -- it does not notice that `f` and `x` arguments are loop invariant
    -- and could be floated, and the whole `f x` expression shared.
    -- If we create a closure with `f` and `x` bound in the environment,
    -- then GHC is smart enough to share computation of `f x`.
    --
    -- For perspective, gauge and criterion < 1.4 mark similar functions as INLINE,
    -- while criterion >= 1.4 switches to NOINLINE.
    -- If we mark `funcToBenchLoop` NOINLINE then benchmark results are slightly larger
    -- (noticeable in bench-fibo), because the loop body is slightly bigger,
    -- since GHC does not unbox numbers or inline `Eq @Word64` dictionary.
    --
    -- This function is called `funcToBenchLoop` instead of, say, `go`,
    -- so it is easier to spot in Core dumps.
    --
    -- Forcing SpecConst optimization with SPEC makes the behaviour of benchmarks
    -- independent of -fspec-constr-count.
    -- multiNLoop :: SPEC -> (a -> b) -> a -> Word64 -> IO t
    multiNLoop !_ f x n
      | n == 1 = evaluate (frc (f x))
      | otherwise = do
          _ <- evaluate (frc (f x))
          multiNLoop SPEC f x (n - 1)
{-# INLINE multiN #-}

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
    (t, !ma) <- lift $ measure m (const a) ()
    modify $ second (Map.insertWith (<>) label t)
    lift ma
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
      (fam label (runPerfT meas p))

-- | run a PerfT and calculate excess performance over the entire computation
slop :: (MonadIO m, Num t, Semigroup t) => Text -> Measure m t -> PerfT m t a -> m (a, Map.Map Text t)
slop l meas p =
  (\((a, m), m') -> (a, m <> Map.insert "slop" (m' Map.! l - Map.foldl' (+) 0 m) m'))
    <$> runPerfT
      meas
      (fam l (runPerfT meas p))

-- | run a multi PerfT and calculate excess performance over the entire computation
slops :: (MonadIO m, Num t, Semigroup t) => Int -> Measure m t -> PerfT m [t] a -> m (a, (Map.Map Text t, Map.Map Text [t]))
slops n meas p =
  (\((a, ms), m') -> (a, (Map.insert "slop" (m' Map.! "outer" - Map.foldl' (+) 0 (fmap sum ms)) m', ms)))
    <$> runPerfT
      meas
      (fam "outer" (runPerfT (repeated n meas) p))
