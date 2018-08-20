[perf-criterion](https://github.com/tonyday567/perf/perf-criterion) example
===========================================================================

[ghc options](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html#flag-reference)
--------------------------------------------------------------------------------------------------------

``` {.sourceCode .literate .haskell}
{-# OPTIONS_GHC -Wall #-}
```

[pragmas](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/lang.html)
------------------------------------------------------------------------------------

``` {.sourceCode .literate .haskell}
-- doctest doesn't look at the cabal file, so you need pragmas here
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
```

[libraries](https://www.stackage.org/)
--------------------------------------

-   [protolude](https://www.stackage.org/package/protolude)
-   [optparse-generic](https://www.stackage.org/package/optparse-generic)

``` {.sourceCode .literate .haskell}
import Protolude
import Options.Generic
import Perf
import Criterion
import Criterion.Main
import Perf.Analysis
import Prelude (error)
```

code
----

-   [hoogle](https://www.stackage.org/package/hoogle)

``` {.sourceCode .literate .haskell}

-- The function we're benchmarking.
fib :: Int -> Int
fib m | m < 0     = error "negative!"
      | otherwise = go m
  where
    go :: Int -> Int
    go 0 = 0
    go 1 = 1
    go n = go (n - 1) + go (n - 2)

-- Our benchmark harness.
main = do
  let n = 1000
  defaultMain
    [ bgroup "fib"
      [ bench "1"  $ whnf fib 1
      , bench "5"  $ whnf fib 5
      , bench "9"  $ whnf fib 9
      , bench "11" $ whnf fib 11
      ]
    , bgroup "fibnf"
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
  (ts1, _) <- ticks n fib 1
  (ts5, _) <- ticks n fib 5
  (ts9, _) <- ticks n fib 9
  (ts11, _) <- ticks n fib 11

  writeFile "other/answer.md" $
    code
      [ "fib 1 * 10:" <> show (fst <$> t1)
      , "fib 5 * 10: " <> show (fst <$> t5)
      , "fib 9 * 10: " <> show (fst <$> t9)
      , "fib 11 * 10: " <> show (fst <$> t11)
      , formatRun "fib 1" 2 $ ts1
      , formatRun "fib 5" 2 $ ts5
      , formatRun "fib 9" 2 $ ts9
      , formatRun "fib 11" 2 $ ts11
      ]

data Opts w = Opts
  { number :: w ::: Maybe Integer <?> "The number you want to product to"
  } deriving (Generic)

instance ParseRecord (Opts Wrapped)

main' :: IO ()
main' = do
  o :: Opts Unwrapped <- unwrapRecord "perf-criterion"
  let n = fromMaybe 1000 (number o)
  pure ()
```

output
------

    fib 1 * 10:[820,120,34,16,16,16,16,16,16,68]
    fib 5 * 10: [452,46,16,16,16,16,16,16,16,52]
    fib 9 * 10: [1246,48,32,14,14,14,14,16,16,70]
    fib 11 * 10: [1686,44,30,16,16,16,16,16,18,54]
    fib 1                     7.08e2   6.60e1   2.80e1   2.58e1   2.73e1
    fib 5                     1.90e2   1.24e2   8.20e1   8.33e1   8.40e1
    fib 9                     6.32e2   5.64e2   4.98e2   5.15e2   5.16e2
    fib 11                    1.47e3   1.35e3   1.33e3   1.31e3   1.31e3
