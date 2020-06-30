{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Criterion
import Criterion.Main
import Criterion.Measurement
import Criterion.Measurement.Types
import NumHask.Prelude
import Perf
import Readme.Format as F
import Readme.Lhs as Lhs
import qualified Prelude as P

-- The function we're benchmarking.
fib :: Int -> Int
fib m
  | m < 0 = P.error "negative!"
  | otherwise = go m
  where
    go :: Int -> Int
    go 0 = 0
    go 1 = 1
    go n = go (n - 1) + go (n - 2)

main :: IO ()
main = do
  let n = 1000
  defaultMain
    [ bgroup
        "fib nf"
        [ bench "1" $ nf fib 1,
          bench "5" $ nf fib 5,
          bench "9" $ nf fib 9,
          bench "11" $ nf fib 11
        ]
    ]
  _ <- warmup 100
  t1 <- sequence $ replicate 10 (tick fib 1)
  t5 <- sequence $ replicate 10 (tick fib 5)
  t9 <- sequence $ replicate 10 (tick fib 9)
  t11 <- sequence $ replicate 10 (tick fib 11)
  -- but ticks seems a-ok
  (ts1, _) <- ticks n fib 1
  (ts5, _) <- ticks n fib 5
  (ts9, _) <- ticks n fib 9
  (ts11, _) <- ticks n fib 11
  -- perf-criterion
  pc1 <- criNF n fib 1
  pc5 <- criNF n fib 5
  pc9 <- criNF n fib 9
  pc11 <- criNF n fib 11
  void
    $ runOutput
      ("perf-examples/examples/perf-criterion.md", GitHubMarkdown)
      ("other/perf-criterion.md", GitHubMarkdown)
    $ do
      output "tick_replicate" $ Native $ Lhs.toList $
        formatRunsTime
          0.38e-9
          3
          [ ("fib 1 (ns)", fst <$> t1),
            ("fib 5 (ns)", fst <$> t5),
            ("fib 9 (ns)", fst <$> t9),
            ("fib 11 (ns)", fst <$> t11)
          ]
      output "perf_runs" $ Native $ Lhs.toList $
        formatRunsTime
          0.38e-9
          3
          [ ("fib 1 (ns)", ts1),
            ("fib 5 (ns)", ts5),
            ("fib 9 (ns)", ts9),
            ("fib 11 (ns)", ts11)
          ]
      output "criterion_runs" $ Native $ Lhs.toList $
        formatCriRuns
          criSpeed
          3
          [ ("fib 1 (ns)", pc1),
            ("fib 5 (ns)", pc5),
            ("fib 9 (ns)", pc9),
            ("fib 11 (ns)", pc11)
          ]

criNF :: (NFData b) => Int -> (a -> b) -> a -> IO Measured
criNF n a b = do
  (m, _) <- Criterion.Measurement.measure (nf a b) (fromIntegral n)
  return m

-- | format the cputime and cycle criterion measures
criRun :: Int -> Text -> Measured -> [Text]
criRun p label m =
  [ label,
    F.secs p cpus,
    precI p cs,
    prec p npc
  ]
  where
    cpus = measCpuTime (rescale m)
    cs = measCycles (rescale m)
    npc = cpus / fromIntegral cs * 1e9

-- | format cpu versus gc timings
criSpeed :: Int -> Text -> Measured -> [Text]
criSpeed p label m =
  [ label,
    prec p (measCpuTime (rescale m)),
    prec p (measMutatorCpuSeconds (rescale m)),
    prec p (measGcCpuSeconds (rescale m)),
    precI p $ measCycles (rescale m)
  ]

precI :: (ToIntegral a Integer) => Int -> a -> Text
precI x = prec x . fromIntegral

formatCriRuns :: (Int -> Text -> Measured -> [Text]) -> Int -> [(Text, Measured)] -> Blocks
formatCriRuns r p runs =
  defaultTextTable
    bootTableAttr
    mempty
    ((,ColWidthDefault) <$> [AlignLeft, AlignRight, AlignRight])
    (["run", "cputimes", "cycles", "nanos per cycle"])
    (fmap (uncurry (r p)) runs)

-- | format the first few results, the median and average
formatRun :: Text -> Int -> Int -> [Cycle] -> [Text]
formatRun label n p xs =
  [label]
    <> (prec p . fromIntegral <$> take n xs)
    <> [ prec p $ average (fromIntegral <$> xs),
         prec p $ percentile 0.5 (fromIntegral <$> xs)
       ]

-- | average and time
formatRunTime :: Text -> Double -> Int -> [Cycle] -> [Text]
formatRunTime label npc p xs =
  [label]
    <> [ prec p $ npc * average (fromIntegral <$> xs),
         prec p $ average (fromIntegral <$> xs)
       ]

formatRuns :: Int -> Int -> [(Text, [Cycle])] -> Blocks
formatRuns n p runs =
  defaultTextTable
    bootTableAttr
    mempty
    ( zip
        ([AlignLeft] <> replicate (n + 2) AlignRight)
        (replicate (n + 3) ColWidthDefault)
    )
    ( ["run"]
        <> take n (["first", "second", "third"] <> ((<> "th") . show <$> [4 :: Int ..]))
        <> ["average", "median"]
    )
    (fmap (\(l, as) -> formatRun l n p as) runs)

formatRunsTime :: Double -> Int -> [(Text, [Cycle])] -> Blocks
formatRunsTime npc p runs =
  defaultTextTable
    bootTableAttr
    mempty
    ((,ColWidthDefault) <$> [AlignLeft, AlignRight, AlignRight])
    ["run", "cputime", "cycles"]
    (fmap (\(l, as) -> formatRunTime l npc p as) runs)
