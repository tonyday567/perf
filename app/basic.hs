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

sumRec :: (Num a) => [a] -> a
sumRec = go 0
  where
    go acc [] = acc
    go acc (x:xs) = go (acc+x) xs

fApp_ :: Int -> ()
fApp_ x = foldl' const () [1 .. x]

fSum_ :: Int -> Int
fSum_ x = sum [1 .. x]

fMono :: [Int] -> Int
fMono xs = foldl' (+) 0 xs

fPoly :: (Num b) => [b] -> b
fPoly xs = foldl' (+) 0 xs

fLambda :: [Int] -> Int
fLambda = \xs -> foldl' (+) 0 xs

reportMulti :: String -> StatType -> IO ([Cycle], b) -> IO ()
reportMulti label s x = x & fmap (fst >>> tickStat s >>> T.unpack >>> (label<>)) & (>>= putStrLn)

main :: IO ()
main = do
  o <- execParser opts
  let !n = optionRuns o
  let !l = optionLength o
  let s = optionStatType o
  let !ls = [1..l]

  replicateM 10 tick_ & fmap (show >>> ("tick_: "<>)) & (>>= putStrLn)
  replicateM n tick_ & fmap (tickStat s >>> T.unpack >>> ("tick_: "<>)) & (>>= putStrLn)
  replicateM 10 (tick (const ()) ()) & fmap (fmap fst >>> show >>> ("const (): "<>)) & (>>= putStrLn)

  multi tick n (const ()) () & reportMulti "const ()|" s

  replicateM n (tickIO (pure ())) & fmap (fmap fst >>> tickStat s >>> T.unpack >>> ("tickIO (pure ()): "<>)) & (>>= putStrLn)

  multi tick n fMono ls & reportMulti "fMono|" s
  multi tick n fPoly ls & reportMulti "fPoly|" s
  multi tick n fLambda ls & reportMulti "fLambda|" s

  multi tickUnsafe n fMono ls & reportMulti "fMono|" s
  multi tickUnsafe n fPoly ls & reportMulti "fPoly|" s
  multi tickUnsafe n fLambda ls & reportMulti "fLambda|" s

  multi tickUnsafe n fSum_ l & reportMulti "multi tickUnsafe fSum_|" s
  ticks n fSum_ l & reportMulti "ticks fSum_|" s
  multi tick n fSum_ l & reportMulti "multi tick fSum_|" s
  multi tickUnsafe n fSum_ l & reportMulti "multi tickUnsafe fSum_|" s

  ticks n fApp_ l & reportMulti "ticks fApp_|" s
  multi tick n fApp_ l & reportMulti "multi tick fApp_|" s
  multi tickUnsafe n fApp_ l & reportMulti "multi tickUnsafe fApp_|" s
