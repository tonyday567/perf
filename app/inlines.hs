{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | basic measurement and callibration
module Main where

import Control.Category
import Control.DeepSeq
import Control.Monad
import Control.Monad.State.Lazy
import Data.FormatN
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Options.Applicative
import Perf
import Prelude hiding ((.))

data Options = Options
  { optionRuns :: Int,
    optionLength :: Int,
    optionAlgoType :: AlgoType,
    optionStatDType :: StatDType
  }
  deriving (Eq, Show)

options :: Parser Options
options =
  Options
    <$> option auto (value 1000 <> long "runs" <> short 'r' <> help "number of runs to perform")
    <*> option auto (value 1000 <> long "length" <> short 'l' <> help "length of list")
    <*> parseAlgo
    <*> parseStatD

opts :: ParserInfo Options
opts =
  info
    (options <**> helper)
    (fullDesc <> progDesc "perf benchmarking" <> header "basic perf callibration")

-- * in-module variations on a tick counter

tickNoPragma :: (a -> b) -> a -> IO (Cycles, b)
tickNoPragma !f !a = do
  !t <- rdtsc
  !a' <- pure $! f a
  !t' <- rdtsc
  pure (Cycles (t' - t), a')

-- | tick where the arguments are lazy, so measurement may include evaluation of thunks that may constitute f and/or a
tickLazyS :: (a -> b) -> a -> IO (Cycles, b)
tickLazyS f a = do
  !t <- rdtsc
  !a' <- pure (f a)
  !t' <- rdtsc
  pure (Cycles (t' - t), a')
{-# INLINE tickLazyS #-}

tickInline :: (a -> b) -> a -> IO (Cycles, b)
tickInline !f !a = do
  !t <- rdtsc
  !a' <- pure (f a)
  !t' <- rdtsc
  pure (Cycles (t' - t), a')
{-# INLINE tickInline #-}

tickInlineable :: (a -> b) -> a -> IO (Cycles, b)
tickInlineable !f !a = do
  !t <- rdtsc
  !a' <- pure (f a)
  !t' <- rdtsc
  pure (Cycles (t' - t), a')
{-# INLINEABLE tickInlineable #-}

-- | tick where the arguments are lazy, so measurement may include evaluation of thunks that may constitute f and/or a
tickNoinline :: (a -> b) -> a -> IO (Cycles, b)
tickNoinline !f !a = do
  !t <- rdtsc
  !a' <- pure (f a)
  !t' <- rdtsc
  pure (Cycles (t' - t), a')
{-# NOINLINE tickNoinline #-}

tickInline1 :: (a -> b) -> a -> IO (Cycles, b)
tickInline1 !f !a = do
  !t <- rdtsc
  !a' <- pure (f a)
  !t' <- rdtsc
  pure (Cycles (t' - t), a')
{-# INLINE [1] tickInline1 #-}

tickInline2 :: (a -> b) -> a -> IO (Cycles, b)
tickInline2 !f !a = do
  !t <- rdtsc
  !a' <- pure (f a)
  !t' <- rdtsc
  pure (Cycles (t' - t), a')
{-# INLINE [2] tickInline2 #-}

tickInline1' :: (a -> b) -> a -> IO (Cycles, b)
tickInline1' !f !a = do
  !t <- rdtsc
  !a' <- pure (f a)
  !t' <- rdtsc
  pure (Cycles (t' - t), a')
{-# INLINE [~1] tickInline1' #-}

tickInline2' :: (a -> b) -> a -> IO (Cycles, b)
tickInline2' !f !a = do
  !t <- rdtsc
  !a' <- pure (f a)
  !t' <- rdtsc
  pure (Cycles (t' - t), a')
{-# INLINE [~2] tickInline2' #-}

multi' :: ((a -> b) -> a -> IO (Cycles, b)) -> Int -> (a -> b) -> a -> IO ([Cycles], b)
multi' tickf n0 f a = fmap (\xs -> (fmap fst xs, snd (head xs))) (replicateM n0 (tickf f a))
{-# INLINEABLE multi' #-}

ticks' :: (NFData a, NFData b) => Int -> (a -> b) -> a -> IO ([Cycles], b)
ticks' n0 f a = fmap (\xs -> (fmap fst xs, snd (head xs))) (replicateM n0 (tickForce f a))
{-# INLINEABLE ticks' #-}

ticksNoPragma :: Int -> (a -> b) -> a -> IO ([Cycles], b)
ticksNoPragma n0 f a = go f a n0 []
  where
    go f' a' n ts
      | n <= 0 = pure (ts, f a)
      | otherwise = do
          (t, _) <- tickNoPragma f a
          go f' a' (n - 1) (t : ts)

ticksLazy :: Int -> (a -> b) -> a -> IO ([Cycles], b)
ticksLazy n0 f a = go f a n0 []
  where
    go f' a' n ts
      | n <= 0 = pure (ts, f a)
      | otherwise = do
          (t, _) <- tickLazy f a
          go f' a' (n - 1) (t : ts)

ticksInline :: Int -> (a -> b) -> a -> IO ([Cycles], b)
ticksInline n0 f a = go f a n0 []
  where
    go f' a' n ts
      | n <= 0 = pure (ts, f a)
      | otherwise = do
          (t, _) <- tickInline f a
          go f' a' (n - 1) (t : ts)

ticksInlineable :: Int -> (a -> b) -> a -> IO ([Cycles], b)
ticksInlineable n0 f a = go f a n0 []
  where
    go f' a' n ts
      | n <= 0 = pure (ts, f a)
      | otherwise = do
          (t, _) <- tickInlineable f a
          go f' a' (n - 1) (t : ts)

ticksNoinline :: Int -> (a -> b) -> a -> IO ([Cycles], b)
ticksNoinline n0 f a = go f a n0 []
  where
    go f' a' n ts
      | n <= 0 = pure (ts, f a)
      | otherwise = do
          (t, _) <- tickNoinline f a
          go f' a' (n - 1) (t : ts)

ticksInline1 :: Int -> (a -> b) -> a -> IO ([Cycles], b)
ticksInline1 n0 f a = go f a n0 []
  where
    go f' a' n ts
      | n <= 0 = pure (ts, f a)
      | otherwise = do
          (t, _) <- tickInline1 f a
          go f' a' (n - 1) (t : ts)

ticksInline2 :: Int -> (a -> b) -> a -> IO ([Cycles], b)
ticksInline2 n0 f a = go f a n0 []
  where
    go f' a' n ts
      | n <= 0 = pure (ts, f a)
      | otherwise = do
          (t, _) <- tickInline2 f a
          go f' a' (n - 1) (t : ts)

ticksInline1' :: Int -> (a -> b) -> a -> IO ([Cycles], b)
ticksInline1' n0 f a = go f a n0 []
  where
    go f' a' n ts
      | n <= 0 = pure (ts, f a)
      | otherwise = do
          (t, _) <- tickInline1' f a
          go f' a' (n - 1) (t : ts)

ticksInline2' :: Int -> (a -> b) -> a -> IO ([Cycles], b)
ticksInline2' n0 f a = go f a n0 []
  where
    go f' a' n ts
      | n <= 0 = pure (ts, f a)
      | otherwise = do
          (t, _) <- tickInline2' f a
          go f' a' (n - 1) (t : ts)

-- | run the various incarnations of tick.
runInlines :: (NFData t, NFData b) => Text -> (t -> b) -> t -> Int -> StatDType -> StateT (Map.Map [Text] Double) IO ()
runInlines l f a n s = do
  addStat [l, "tick"] . statD s =<< lift (fst <$> multi tick n f a)
  addStat [l, "tickWHNF"] . statD s =<< lift (fst <$> multi tickWHNF n f a)
  addStat [l, "tickLazy"] . statD s =<< lift (fst <$> multi tickLazy n f a)
  addStat [l, "tickForce"] . statD s =<< lift (fst <$> multi tickForce n f a)
  addStat [l, "tickForceArgs"] . statD s =<< lift (fst <$> multi tickForceArgs n f a)
  addStat [l, "stepTime"] . statD s =<< lift (snd . head . Map.toList <$> execPerfT (toMeasureN n stepTime) (f |$| a))
  addStat [l, "times"] . statD s =<< lift (snd . head . Map.toList <$> execPerfT (times n) (f |$| a))
  addStat [l, "ticksNoPragma"] . statD s =<< lift (fst <$> ticksNoPragma n f a)
  addStat [l, "ticksLazy"] . statD s =<< lift (fst <$> ticksLazy n f a)
  addStat [l, "ticksInlineable"] . statD s =<< lift (fst <$> ticksInlineable n f a)
  addStat [l, "ticksNoinline"] . statD s =<< lift (fst <$> ticksNoinline n f a)
  addStat [l, "ticksInline1"] . statD s =<< lift (fst <$> ticksInline1 n f a)
  addStat [l, "ticksInline2"] . statD s =<< lift (fst <$> ticksInline2 n f a)
  addStat [l, "ticksInline1'"] . statD s =<< lift (fst <$> ticksInline1' n f a)
  addStat [l, "ticksInline2'"] . statD s =<< lift (fst <$> ticksInline2' n f a)
  addStat [l, "multi_tickNoPragma"] . statD s =<< lift (fst <$> multi tickNoPragma n f a)
  addStat [l, "multi_tickInlineable"] . statD s =<< lift (fst <$> multi tickInlineable n f a)
  addStat [l, "multi_tickNoinline"] . statD s =<< lift (fst <$> multi tickNoinline n f a)
  addStat [l, "multi_tickInline1"] . statD s =<< lift (fst <$> multi tickInline1 n f a)
  addStat [l, "multi_tickInline2"] . statD s =<< lift (fst <$> multi tickInline2 n f a)
  addStat [l, "multi_tickInline1'"] . statD s =<< lift (fst <$> multi tickInline1' n f a)
  addStat [l, "multi_tickInline2'"] . statD s =<< lift (fst <$> multi tickInline2' n f a)

main :: IO ()
main = do
  o <- execParser opts
  let !n = optionRuns o
  let !l = optionLength o
  let !ls = [1 .. l]
  let s = optionStatDType o
  let !a = optionAlgoType o
  _ <- warmup 100

  m <- flip execStateT Map.empty $
    case a of
      AlgoFuseSum -> runInlines "fuseSum" fuseSum l n s
      AlgoFuseConst -> runInlines "fuseConst" fuseConst l n s
      AlgoRecSum -> runInlines "recSum" recSum ls n s
      AlgoMonoSum -> runInlines "monoSum" monoSum ls n s
      AlgoPolySum -> runInlines "polySum" polySum ls n s
      AlgoLambdaSum -> runInlines "lambdaSum" lambdaSum ls n s

  printOrg (fmap (expt (Just 3)) m)
