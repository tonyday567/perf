{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}

-- | basic measurement and callibration

module Main where

import Prelude hiding ((.))
import Perf.Cycle as Perf
import qualified Data.Text as T
import Data.Function
import Control.Category
import Control.Monad
import Options.Applicative
import Data.Foldable

data RunType = RunBasic deriving (Eq, Show)

data StatType = StatAverage | StatMedian | StatBest deriving (Eq, Show)

data AlgoType = AlgoFSum | AlgoFConst | AlgoMonoSum | AlgoPolySum | AlgoLambdaSum deriving (Eq, Show)

data Options = Options
  { optionRuns :: Int,
    optionBasic :: Bool,
    optionLength :: Int,
    optionStatType :: StatType,
    optionAlgoType :: AlgoType
  } deriving (Eq, Show)

options :: Parser Options
options = Options <$>
  option auto (long "runs" <> short 'r' <> help "number of runs to perform") <*>
  switch (long "include basic effect measurements" <> short 'b') <*>
  option auto (long "length" <> short 'l' <> help "length of list") <*>
  stat <*>
  algo

opts :: ParserInfo Options
opts = info (options <**> helper)
  (fullDesc <> progDesc "perf benchmarking" <> header "basic perf callibration")

stat :: Parser StatType
stat =
  flag' StatBest (long "best" <> help "report upper decile") <|>
  flag' StatMedian (long "median" <> help "report median") <|>
  flag' StatAverage (long "average" <> help "report average") <|>
  pure StatAverage

algo :: Parser AlgoType
algo =
  flag' AlgoFSum (long "fsum" <> help "fused sum") <|>
  flag' AlgoFConst (long "fconst" <> help "fused const") <|>
  flag' AlgoMonoSum (long "monosum" <> help "monomorphic sum") <|>
  flag' AlgoPolySum (long "polysum" <> help "polymorphic sum") <|>
  flag' AlgoLambdaSum (long "lambdasum" <> help "lambdaed sum") <|>
  pure AlgoFSum

tickStat :: StatType -> [Cycle] -> T.Text
tickStat StatBest = tenth
tickStat StatMedian = median
tickStat StatAverage = average

fApp_ :: Int -> Int
fApp_ x = foldl' const 0 [1 .. x]

fSum_ :: Int -> Int
fSum_ x = sum [1 .. x]

fMono :: [Int] -> Int
fMono xs = foldl' (+) 0 xs

fPoly :: (Num b) => [b] -> b
fPoly xs = foldl' (+) 0 xs

fLambda :: [Int] -> Int
fLambda = \xs -> foldl' (+) 0 xs

tickSameFile :: (a -> b) -> a -> IO (Cycle, b)
tickSameFile !f !a = do
  !t <- rdtsc
  !a' <- pure (f a)
  !t' <- rdtsc
  pure (t' - t, a')
{-# INLINE tickSameFile #-}

ticksRec :: ((a -> b) -> a -> IO (Cycle, b)) -> Int -> (a -> b) -> a -> IO ([Cycle], b)
ticksRec tickf n0 f a = go f a n0 []
  where
    go f' a' n ts
      | n <= 0 = pure (ts, f a)
      | otherwise = do
        (t, _) <- tickf f a
        go f' a' (n - 1) (t:ts)

ticksR :: ((a -> b) -> a -> IO (Cycle, b)) -> Int -> (a -> b) -> a -> IO ([Cycle], b)
ticksR tickf n0 f a = fmap (\xs -> (fmap fst xs, snd (head xs))) (replicateM n0 (tickf f a))

ticksSameFile :: Int -> (a -> b) -> a -> IO ([Cycle], b)
ticksSameFile n0 f a = go f a n0 []
  where
    go f' a' n ts
      | n <= 0 = pure (ts, f a)
      | otherwise = do
        (t, _) <- tickSameFile f a
        go f' a' (n - 1) (t:ts)

main :: IO ()
main = do
  o <- execParser opts
  let !n = optionRuns o
  let !l = optionLength o
  let s = optionStatType o
  let a = optionAlgoType o
  print a
  _ <- warmup 100
{-
  let (tf, ta) = case a of
        -- AlgoFSum -> (fSum_, l)
        -- AlgoFConst -> (fApp_, l)
        AlgoMonoSum -> (fMono, [1..l])
        AlgoPolySum -> (fPoly, [1..l])
        AlgoLambdaSum -> (fLambda, [1..l])
        _ -> (fMono, [1..l])
-}

  let (tf, ta) = case a of
        AlgoFSum -> (fSum_, l)
        AlgoFConst -> (fApp_, l)
        -- AlgoMonoSum -> (fMono, [1..l])
        -- AlgoPolySum -> (fPoly, [1..l])
        -- AlgoLambdaSum -> (fLambda, [1..l])
        _ -> (fSum_, l)
  replicateM n (Perf.tick tf ta) & fmap (fmap fst >>> tickStat s >>> T.unpack >>> ("replicateM Perf.tick "<>)) & (>>= putStrLn)
  replicateM n (tickSameFile tf ta) & fmap (fmap fst >>> tickStat s >>> T.unpack >>> ("replicateM tickSameFile "<>)) & (>>= putStrLn)

  Perf.ticks n tf ta & fmap (fst >>> tickStat s >>> T.unpack >>> ("Perf.ticks " <>)) & (>>= putStrLn)
  Perf.multi Perf.tick n tf ta & fmap (fst >>> tickStat s >>> T.unpack >>> ("Perf.multi Perf.tick " <>)) & (>>= putStrLn)
  Perf.multi tickSameFile n tf ta & fmap (fst >>> tickStat s >>> T.unpack >>> ("Perf.multi tickSameFile " <>)) & (>>= putStrLn)

  ticksSameFile n tf ta & fmap (fst >>> tickStat s >>> T.unpack >>> ("ticksSameFile " <>)) & (>>= putStrLn)
  ticksR tickSameFile n tf ta & fmap (fst >>> tickStat s >>> T.unpack >>> ("ticksR tickSameFile " <>)) & (>>= putStrLn)
  ticksRec tickSameFile n tf ta & fmap (fst >>> tickStat s >>> T.unpack >>> ("ticksRec tickSameFile " <>)) & (>>= putStrLn)
  ticksR tick n tf ta & fmap (fst >>> tickStat s >>> T.unpack >>> ("ticksR tick " <>)) & (>>= putStrLn)
  ticksRec tick n tf ta & fmap (fst >>> tickStat s >>> T.unpack >>> ("ticksRec tick " <>)) & (>>= putStrLn)
