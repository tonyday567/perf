{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

-- | 'PerfT' is a monad transformer designed to collect performance information.
-- The transformer can be used to add performance measurent to an existing code base using 'Measure's.
--
-- For example, here's some code doing some cheesey stuff:
--
-- >   -- prior to Perfification
-- >   result <- do
-- >       txt <- readFile "examples/examples.hs"
-- >       let n = Text.length txt
-- >       let x = foldl' (+) 0 [1..n]
-- >       putStrLn $ "sum of one to number of characters is: " <>
-- >           (show x :: Text)
-- >       pure (n, x)
--
-- And here's the code after 'Perf'ification, measuring performance of the components.
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
module Perf
  ( PerfT
  , Perf
  , perf
  , perfN
  , runPerfT
  , evalPerfT
  , execPerfT
  , module Perf.Cycle
  , module Perf.Measure
  ) where

import qualified Data.Map as Map
import NumHask.Prelude
import Perf.Cycle
import Perf.Measure

-- | PerfT is polymorphic in the type of measurement being performed.
-- The monad stores and produces a Map of labelled measurement values
newtype PerfT m b a = PerfT
  { runPerf_ :: StateT (Map.Map Text b) m a
  } deriving (Functor, Applicative, Monad)

-- | The obligatory transformer over Identity
type Perf b a = PerfT Identity b a

instance (MonadIO m) => MonadIO (PerfT m b) where
  liftIO = PerfT . liftIO

-- | Lift a monadic computation to a PerfT m, providing a label and a 'Measure'.
perf :: (MonadIO m, Additive b) => Text -> Measure m b -> m a -> PerfT m b a
perf label m a =
  PerfT $ do
    st <- get
    (m', a') <- lift $ runMeasure m a
    put $ Map.insertWith (+) label m' st
    return a'

-- | Lift a monadic computation to a PerfT m, and carry out the computation multiple times.
perfN ::
     (MonadIO m, Semigroup b, Monoid b)
  => Int
  -> Text
  -> Measure m b
  -> m a
  -> PerfT m b a
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
runPerfT :: PerfT m b a -> m (a, Map.Map Text b)
runPerfT p = flip runStateT Map.empty $ runPerf_ p

-- | Consume the PerfT layer and return the original monadic result.
-- Fingers crossed, PerfT structure should be completely compiled away.
--
-- >>> result <- evalPerfT $ perf "sum" cycles (pure $ foldl' (+) 0 [0..10000])
--
-- > 50005000
evalPerfT :: (Monad m) => PerfT m b a -> m a
evalPerfT p = flip evalStateT Map.empty $ runPerf_ p

-- | Consume a PerfT layer and return the measurement.
--
-- >>> cs <- execPerfT $ perf "sum" cycles (pure $ foldl' (+) 0 [0..10000])
--
-- > fromList [("sum",562028)]
execPerfT :: (Monad m) => PerfT m b a -> m (Map.Map Text b)
execPerfT p = flip execStateT Map.empty $ runPerf_ p
