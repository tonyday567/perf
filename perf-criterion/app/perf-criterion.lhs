[perf-criterion](https://github.com/tonyday567/perf/perf-criterion) example
===

[ghc options](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html#flag-reference)
---

\begin{code}
{-# OPTIONS_GHC -Wall #-}
\end{code}

[pragmas](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/lang.html)
---

\begin{code}
-- doctest doesn't look at the cabal file, so you need pragmas here
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
\end{code}

[libraries](https://www.stackage.org/)
---

- [protolude](https://www.stackage.org/package/protolude)
- [optparse-generic](https://www.stackage.org/package/optparse-generic)

\begin{code}
import Protolude hiding ((%))
import Options.Generic
import Perf
import Criterion
import Criterion.Main
import Criterion.Measurement
import Perf.Analysis
import qualified Perf.Criterion as C
import Prelude (error)
import Formatting
import Data.Scientific
\end{code}

code
---

- [hoogle](https://www.stackage.org/package/hoogle)

\begin{code}

-- The function we're benchmarking.
fib :: Int -> Int
fib m | m < 0     = error "negative!"
      | otherwise = go m
  where
    go :: Int -> Int
    go 0 = 0
    go 1 = 1
    go n = go (n - 1) + go (n - 2)

-- | average time and cycles
formatRun' :: (Integral a) => Text -> Int -> [a] -> Text
formatRun' label p xs =
  sformat
  ((right 24 ' ' %. stext) %
    (left 9 ' ' %. prec p) %
    (left 9 ' ' %. prec p))
  label
  (fromFloatDigits $ av $ ns <$> xs)
  (fromFloatDigits $ average xs)
  where
    ns = ((1e-9 :: Double)*) . (0.38*) . fromIntegral
    av xs = sum xs / fromIntegral (length xs)


-- Our benchmark harness.
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

  -- sequence . replicate n memo'izes
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
  pc1 <- C.criNF n fib 1
  pc5 <- C.criNF n fib 5
  pc9 <- C.criNF n fib 9
  pc11 <- C.criNF n fib 11



  writeFile "other/answer.md" $
    code
      [ formatRun' "fib 1 (ns)" 2 $ ts1
      , formatRun' "fib 5 (ns)" 2 $ ts5
      , formatRun' "fib 9 (ns)" 2 $ ts9
      , formatRun' "fib 11 (ns)" 2 $ ts11
      , C.formatRun "cri: fib 1 (ns)" 2 $ pc1
      , C.formatRun "fib 5 (ns)" 2 $ pc5
      , C.formatRun "fib 9 (ns)" 2 $ pc9
      , C.formatRun "fib 11 (ns)" 2 $ pc11
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

\end{code}

output
---

```include
other/answer.md
```

