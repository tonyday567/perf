[perf-criterion](https://github.com/tonyday567/perf/perf-criterion) example
===

\begin{code}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import Protolude
import Perf
import Criterion
import Criterion.Main
import Perf.Analysis
import qualified Perf.Criterion as C
import Prelude (error)
import Readme.Lhs

-- The function we're benchmarking.
fib :: Int -> Int
fib m | m < 0     = error "negative!"
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
    [
      bgroup "fib nf"
      [ bench "1"  $ nf fib 1
      , bench "5"  $ nf fib 5
      , bench "9"  $ nf fib 9
      , bench "11" $ nf fib 11
      ]
    ]

  _ <- warmup 100

  t1 <- sequence $ replicate 10 (tick fib 1)
  t5 <- sequence $ replicate 10 (tick fib 5)
  t9 <- sequence $ replicate 10 (tick fib 9)
  t11 <- sequence $ replicate 10 (tick fib 11)

\end{code}

`replicate . ticks` seems to memo'ize

```{.output .tick_replicate}
```

\begin{code}
  -- but ticks seems a-ok
  (ts1, _) <- ticks n fib 1
  (ts5, _) <- ticks n fib 5
  (ts9, _) <- ticks n fib 9
  (ts11, _) <- ticks n fib 11

\end{code}

perf runs

```{.output .perf_runs}
```

\begin{code}

  -- perf-criterion
  pc1 <- C.criNF n fib 1
  pc5 <- C.criNF n fib 5
  pc9 <- C.criNF n fib 9
  pc11 <- C.criNF n fib 11

\end{code}

criterion runs

```{.output .criterion_runs}
```

\begin{code}


  void $ runOutput
    ("perf-criterion/app/perf-criterion.lhs", LHS)
    ("criterion.md", GitHubMarkdown) $ do
    output "tick_replicate" $ Native $ (:[]) $ formatRunsTime 0.38e-9 3
      [ ("fib 1 (ns)", fst <$> t1)
      , ("fib 5 (ns)", fst <$> t5)
      , ("fib 9 (ns)", fst <$> t9)
      , ("fib 11 (ns)", fst <$> t11)
      ]
    output "perf_runs" $ Native $ (:[]) $ formatRunsTime 0.38e-9 3
      [ ("fib 1 (ns)", ts1)
      , ("fib 5 (ns)", ts5)
      , ("fib 9 (ns)", ts9)
      , ("fib 11 (ns)", ts11)
      ]
    output "criterion_runs" $ Native $ (:[]) $ C.formatCriRuns C.criSpeed 3
      [ ("fib 1 (ns)", pc1)
      , ("fib 5 (ns)", pc5)
      , ("fib 9 (ns)", pc9)
      , ("fib 11 (ns)", pc11)
      ]

\end{code}
