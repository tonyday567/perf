{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}

-- | basic measurement and callibration

module Main where

import Prelude hiding ((.))
import Perf.Cycle
import qualified Data.Text as T
import Data.Function
import Control.Category
import Control.Monad
import Options.Applicative
import Data.Foldable (foldl')

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

fa_ :: [a] -> ()
fa_ [] = ()
fa_ (_:xs) = fa_ xs

sum' :: (Num a) => [a] -> a
sum' = go 0
  where
    go acc [] = acc
    go acc (x:xs) = go (acc+x) xs

fApp_ :: Int -> ()
fApp_ x = foldl' const () [1 .. x]

fSum_ :: Int -> Int
fSum_ x = sum [1 .. x]

fMono :: [Int] -> Int
fMono xs = foldl' (+) 0 xs

fPoly :: (Enum b, Num b) => [b] -> b
fPoly xs = foldl' (+) 0 xs

fLambda :: [Int] -> Int
fLambda = \xs -> foldl' (+) 0 xs

main :: IO ()
main = do
  o <- execParser opts
  let !n = optionRuns o
  let !l = optionLength o
  let s = optionStatType o
  _ <- warmup 100

  replicateM 10 tick_ & fmap (show >>> ("tick_: "<>)) & (>>= putStrLn)
  replicateM n tick_ & fmap (tickStat s >>> T.unpack >>> ("tick_: "<>)) & (>>= putStrLn)
  replicateM 10 (tick (const ()) ()) & fmap (fmap fst >>> show >>> ("const (): "<>)) & (>>= putStrLn)
  replicateM n (tick (const ()) ()) & fmap (fmap fst >>> tickStat s >>> T.unpack >>> ("tick (const ()): "<>)) & (>>= putStrLn)
  replicateM n (tickIO (pure ())) & fmap (fmap fst >>> tickStat s >>> T.unpack >>> ("tickIO (pure ()): "<>)) & (>>= putStrLn)

  replicateM n (tick fa_ [1..l::Int]) & fmap (fmap fst >>> tickStat s >>> T.unpack >>> ("fa: "<>)) & (>>= putStrLn)

  replicateM n (tick sum [1..l::Int]) & fmap (fmap fst >>> tickStat s >>> T.unpack >>> ("sum: "<>)) & (>>= putStrLn)
  replicateM n (tick sum' [1..l::Int]) & fmap (fmap fst >>> tickStat s >>> T.unpack >>> ("sum': "<>)) & (>>= putStrLn)

  -- seeing if printing the answer makes a difference (it really shouldn't)
  -- replicateM n (tick fSum_ l) & fmap (\xs -> "fSum_ & result: " <> T.unpack (tickStat s (fmap fst xs)) <> " " <> show (sum (fmap snd xs))) & (>>= putStrLn)

  replicateM n (tick fApp_ l) & fmap (fmap fst >>> tickStat s >>> T.unpack >>> ("fApp_: "<>)) & (>>= putStrLn)
  replicateM n (tick fSum_ l) & fmap (fmap fst >>> tickStat s >>> T.unpack >>> ("fSum_ "<>)) & (>>= putStrLn)

  replicateM n (tick fMono [1..l]) & fmap (fmap fst >>> tickStat s >>> T.unpack >>> ("fMono: "<>)) & (>>= putStrLn)
  replicateM n (tick fPoly [1..l]) & fmap (fmap fst >>> tickStat s >>> T.unpack >>> ("fPoly: "<>)) & (>>= putStrLn)
  replicateM n (tick fLambda [1..l]) & fmap (fmap fst >>> tickStat s >>> T.unpack >>> ("fLambda: "<>)) & (>>= putStrLn)

{-
  -- The various ticks
  ticksi n sum [1..l] & fmap (fst >>> tickStat s >>> T.unpack >>> ("ticksi sum: " <>)) & (>>= putStrLn)
  ticks n sum [1..l] & fmap (fst >>> tickStat s >>> T.unpack >>> ("ticks sum: " <>)) & (>>= putStrLn)
  ticksi n sum [1..l] & fmap (\(c,x) -> "ticksi sum & result: " <> T.unpack (tickStat s c) <> " " <> show x) & (>>= putStrLn)

  -- The various ticks
  ticksi n fSum_ l & fmap (fst >>> tickStat s >>> T.unpack >>> ("ticksi fSum_: " <>)) & (>>= putStrLn)
  ticks n fSum_ l & fmap (fst >>> tickStat s >>> T.unpack >>> ("ticks fSum_: " <>)) & (>>= putStrLn)

-}
