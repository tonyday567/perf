{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}

-- | basic measurement and callibration

module Main where

import Prelude hiding ((.))
import Perf.Cycle hiding (ticks, tickLazy)
import qualified Perf.Cycle as Perf
import qualified Data.Text as T
import Data.Function
import Control.Category
import Control.Monad
import Options.Applicative
import System.CPUTime.Rdtsc

data RunType = RunBasic deriving (Eq, Show)

data StatType = StatAverage | StatMedian | StatBest deriving (Eq, Show)

data Options = Options
  { optionRuns :: Int,
    optionBasic :: Bool,
    optionLength :: Int,
    optionStatType :: StatType
  } deriving (Eq, Show)

options :: Parser Options
options = Options <$>
  option auto (long "runs" <> short 'r' <> help "number of runs to perform") <*>
  switch (long "include basic effect measurements" <> short 'b') <*>
  option auto (long "length" <> short 'l' <> help "length of list") <*>
  stat

opts :: ParserInfo Options
opts = info (options <**> helper)
  (fullDesc <> progDesc "perf benchmarking" <> header "basic perf callibration")

stat :: Parser StatType
stat =
  flag' StatBest (long "best" <> help "report upper decile") <|>
  flag' StatMedian (long "median" <> help "report median") <|>
  flag' StatAverage (long "average" <> help "report average") <|>
  pure StatAverage

tickStat :: StatType -> [Cycle] -> T.Text
tickStat StatBest = tenth
tickStat StatMedian = median
tickStat StatAverage = average

fSum_ :: Int -> Int
fSum_ x = sum [1 .. x]

tickNoPragma :: (a -> b) -> a -> IO (Cycle, b)
tickNoPragma !f !a = do
  !t <- rdtsc
  !a' <- pure (f a)
  !t' <- rdtsc
  pure (t' - t, a')

-- | tick where the arguments are lazy, so measurement may include evaluation of thunks that may constitute f and/or a
tickLazy :: (a -> b) -> a -> IO (Cycle, b)
tickLazy f a = do
  !t <- rdtsc
  !a' <- pure (f a)
  !t' <- rdtsc
  pure (t' - t, a')
{-# INLINE tickLazy #-}

tickInline :: (a -> b) -> a -> IO (Cycle, b)
tickInline !f !a = do
  !t <- rdtsc
  !a' <- pure (f a)
  !t' <- rdtsc
  pure (t' - t, a')
{-# INLINE tickInline #-}

tickInlineable :: (a -> b) -> a -> IO (Cycle, b)
tickInlineable !f !a = do
  !t <- rdtsc
  !a' <- pure (f a)
  !t' <- rdtsc
  pure (t' - t, a')
{-# INLINEABLE tickInlineable #-}

-- | tick where the arguments are lazy, so measurement may include evaluation of thunks that may constitute f and/or a
tickNoinline :: (a -> b) -> a -> IO (Cycle, b)
tickNoinline !f !a = do
  !t <- rdtsc
  !a' <- pure (f a)
  !t' <- rdtsc
  pure (t' - t, a')
{-# NOINLINE tickNoinline #-}

tickInline1 :: (a -> b) -> a -> IO (Cycle, b)
tickInline1 !f !a = do
  !t <- rdtsc
  !a' <- pure (f a)
  !t' <- rdtsc
  pure (t' - t, a')
{-# INLINE [1] tickInline1 #-}

tickInline2 :: (a -> b) -> a -> IO (Cycle, b)
tickInline2 !f !a = do
  !t <- rdtsc
  !a' <- pure (f a)
  !t' <- rdtsc
  pure (t' - t, a')
{-# INLINE [2] tickInline2 #-}

tickInline1' :: (a -> b) -> a -> IO (Cycle, b)
tickInline1' !f !a = do
  !t <- rdtsc
  !a' <- pure (f a)
  !t' <- rdtsc
  pure (t' - t, a')
{-# INLINE [~1] tickInline1' #-}

tickInline2' :: (a -> b) -> a -> IO (Cycle, b)
tickInline2' !f !a = do
  !t <- rdtsc
  !a' <- pure (f a)
  !t' <- rdtsc
  pure (t' - t, a')
{-# INLINE [~2] tickInline2' #-}

ticks :: ((a -> b) -> a -> IO (Cycle, b)) -> Int -> (a -> b) -> a -> IO ([Cycle], b)
ticks tickf n0 f a = go f a n0 []
  where
    go f' a' n ts
      | n <= 0 = pure (ts, f a)
      | otherwise = do
        (t, _) <- tickf f a
        go f' a' (n - 1) (t:ts)

ticks' :: ((a -> b) -> a -> IO (Cycle, b)) -> Int -> (a -> b) -> a -> IO ([Cycle], b)
ticks' tickf n0 f a = fmap (\xs -> (fmap fst xs, snd (head xs))) (replicateM n0 (tickf f a))

ticksNoPragma :: Int -> (a -> b) -> a -> IO ([Cycle], b)
ticksNoPragma n0 f a = go f a n0 []
  where
    go f' a' n ts
      | n <= 0 = pure (ts, f a)
      | otherwise = do
        (t, _) <- tickNoPragma f a
        go f' a' (n - 1) (t:ts)

ticksLazy :: Int -> (a -> b) -> a -> IO ([Cycle], b)
ticksLazy n0 f a = go f a n0 []
  where
    go f' a' n ts
      | n <= 0 = pure (ts, f a)
      | otherwise = do
        (t, _) <- tickLazy f a
        go f' a' (n - 1) (t:ts)

ticksInline :: Int -> (a -> b) -> a -> IO ([Cycle], b)
ticksInline n0 f a = go f a n0 []
  where
    go f' a' n ts
      | n <= 0 = pure (ts, f a)
      | otherwise = do
        (t, _) <- tickInline f a
        go f' a' (n - 1) (t:ts)

ticksInlineable :: Int -> (a -> b) -> a -> IO ([Cycle], b)
ticksInlineable n0 f a = go f a n0 []
  where
    go f' a' n ts
      | n <= 0 = pure (ts, f a)
      | otherwise = do
        (t, _) <- tickInlineable f a
        go f' a' (n - 1) (t:ts)

ticksNoinline :: Int -> (a -> b) -> a -> IO ([Cycle], b)
ticksNoinline n0 f a = go f a n0 []
  where
    go f' a' n ts
      | n <= 0 = pure (ts, f a)
      | otherwise = do
        (t, _) <- tickNoinline f a
        go f' a' (n - 1) (t:ts)

ticksInline1 :: Int -> (a -> b) -> a -> IO ([Cycle], b)
ticksInline1 n0 f a = go f a n0 []
  where
    go f' a' n ts
      | n <= 0 = pure (ts, f a)
      | otherwise = do
        (t, _) <- tickInline1 f a
        go f' a' (n - 1) (t:ts)

ticksInline2 :: Int -> (a -> b) -> a -> IO ([Cycle], b)
ticksInline2 n0 f a = go f a n0 []
  where
    go f' a' n ts
      | n <= 0 = pure (ts, f a)
      | otherwise = do
        (t, _) <- tickInline2 f a
        go f' a' (n - 1) (t:ts)

ticksInline1' :: Int -> (a -> b) -> a -> IO ([Cycle], b)
ticksInline1' n0 f a = go f a n0 []
  where
    go f' a' n ts
      | n <= 0 = pure (ts, f a)
      | otherwise = do
        (t, _) <- tickInline1' f a
        go f' a' (n - 1) (t:ts)

ticksInline2' :: Int -> (a -> b) -> a -> IO ([Cycle], b)
ticksInline2' n0 f a = go f a n0 []
  where
    go f' a' n ts
      | n <= 0 = pure (ts, f a)
      | otherwise = do
        (t, _) <- tickInline2' f a
        go f' a' (n - 1) (t:ts)

main :: IO ()
main = do
  o <- execParser opts
  let !n = optionRuns o
  let !l = optionLength o
  let s = optionStatType o
  _ <- warmup 100

  replicateM n (Perf.tick fSum_ l) & fmap (fmap fst >>> tickStat s >>> T.unpack >>> ("Perf.tick "<>)) & (>>= putStrLn)

  replicateM n (tickNoPragma fSum_ l) & fmap (fmap fst >>> tickStat s >>> T.unpack >>> ("tickNoPragma "<>)) & (>>= putStrLn)
  replicateM n (tickLazy fSum_ l) & fmap (fmap fst >>> tickStat s >>> T.unpack >>> ("tickLazy "<>)) & (>>= putStrLn)
  replicateM n (tickInline fSum_ l) & fmap (fmap fst >>> tickStat s >>> T.unpack >>> ("tickInline "<>)) & (>>= putStrLn)
  replicateM n (tickInlineable fSum_ l) & fmap (fmap fst >>> tickStat s >>> T.unpack >>> ("tickInlineable "<>)) & (>>= putStrLn)
  replicateM n (tickNoinline fSum_ l) & fmap (fmap fst >>> tickStat s >>> T.unpack >>> ("tickNoinline "<>)) & (>>= putStrLn)

  Perf.ticks n fSum_ l & fmap (fst >>> tickStat s >>> T.unpack >>> ("Perf.ticks " <>)) & (>>= putStrLn)
  Perf.multi Perf.tick n fSum_ l & fmap (fst >>> tickStat s >>> T.unpack >>> ("Perf.multi Perf.tick " <>)) & (>>= putStrLn)

  ticksNoPragma n fSum_ l & fmap (fst >>> tickStat s >>> T.unpack >>> ("ticksNoPragma " <>)) & (>>= putStrLn)
  ticksLazy n fSum_ l & fmap (fst >>> tickStat s >>> T.unpack >>> ("ticksLazy " <>)) & (>>= putStrLn)
  ticksInline n fSum_ l & fmap (fst >>> tickStat s >>> T.unpack >>> ("ticksInline " <>)) & (>>= putStrLn)
  ticksInlineable n fSum_ l & fmap (fst >>> tickStat s >>> T.unpack >>> ("ticksInlineable " <>)) & (>>= putStrLn)
  ticksNoinline n fSum_ l & fmap (fst >>> tickStat s >>> T.unpack >>> ("ticksNoinline " <>)) & (>>= putStrLn)
  ticksInline1 n fSum_ l & fmap (fst >>> tickStat s >>> T.unpack >>> ("ticksInline1 " <>)) & (>>= putStrLn)
  ticksInline2 n fSum_ l & fmap (fst >>> tickStat s >>> T.unpack >>> ("ticksInline2 " <>)) & (>>= putStrLn)
  ticksInline1' n fSum_ l & fmap (fst >>> tickStat s >>> T.unpack >>> ("ticksInline1' " <>)) & (>>= putStrLn)
  ticksInline2' n fSum_ l & fmap (fst >>> tickStat s >>> T.unpack >>> ("ticksInline2' " <>)) & (>>= putStrLn)

  ticks' tickNoPragma n fSum_ l & fmap (fst >>> tickStat s >>> T.unpack >>> ("ticks tickNoPragma " <>)) & (>>= putStrLn)
  ticks' tickLazy n fSum_ l & fmap (fst >>> tickStat s >>> T.unpack >>> ("ticks tickLazy " <>)) & (>>= putStrLn)
  ticks' tickInline n fSum_ l & fmap (fst >>> tickStat s >>> T.unpack >>> ("ticks tickInline " <>)) & (>>= putStrLn)
  ticks' tickInlineable n fSum_ l & fmap (fst >>> tickStat s >>> T.unpack >>> ("ticks tickInlineable " <>)) & (>>= putStrLn)
  ticks' tickNoinline n fSum_ l & fmap (fst >>> tickStat s >>> T.unpack >>> ("ticks tickNoinline " <>)) & (>>= putStrLn)
  ticks' tickInline1 n fSum_ l & fmap (fst >>> tickStat s >>> T.unpack >>> ("ticks tickInline1 " <>)) & (>>= putStrLn)
  ticks' tickInline2 n fSum_ l & fmap (fst >>> tickStat s >>> T.unpack >>> ("ticks tickInline2 " <>)) & (>>= putStrLn)
  ticks' tickInline1' n fSum_ l & fmap (fst >>> tickStat s >>> T.unpack >>> ("ticks tickInline1' " <>)) & (>>= putStrLn)
  ticks' tickInline2' n fSum_ l & fmap (fst >>> tickStat s >>> T.unpack >>> ("ticks tickInline2' " <>)) & (>>= putStrLn)
