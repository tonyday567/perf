<meta charset="utf-8">
<link rel="stylesheet" href="https://tonyday567.github.io/other/lhs.css">
<script type="text/javascript" async
  src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML">
</script>
[perf](https://tonyday567.github.io/perf/index.html) [![Build Status](https://travis-ci.org/tonyday567/perf.png)](https://travis-ci.org/tonyday567/perf)
========================================================================================================================================================

If you want to make stuff very fast in haskell, you need to dig down
below the criterion abstraction-level and start counting cycles using
the [rdtsc](https://en.wikipedia.org/wiki/Time_Stamp_Counter) register
on x86.

These examples are experiments in measuring cycles (or ticks), a
development of intuition about what is going on at the very fast level.

The interface is subject to change as intuition develops and rabbit
holes explored.

``` {.sourceCode .literate .haskell}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
import Data.Text (pack, intercalate)
import Data.Text.IO (writeFile)
import Formatting
import Protolude hiding ((%), intercalate)
import Data.List ((!!))
import Data.TDigest
import System.Random.MWC.Probability
import Options.Generic
import qualified Control.Foldl as L
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as S
```

The examples below mostly use `Perf.Cycles`. There is also a monad layer
in `Perf` which has been used in `other/summing.lhs`.

``` {.sourceCode .literate .haskell}
import Perf
```

All the imports that are needed for charts

``` {.sourceCode .literate .haskell}
import Chart.Unit
import Chart.Types
import Chart.Range
import Data.Default
import Diagrams.Prelude
```

command line
------------

``` {.sourceCode .literate .haskell}
data Opts = Opts
  { runs :: Maybe Int    -- <?> "number of runs"
  , sumTo :: [Double] -- <?> "sum to this number"
  , chartNum :: Maybe Int
  , truncAt :: Maybe Double
  }
  deriving (Generic, Show)

instance ParseRecord Opts
```

main
----

``` {.sourceCode .literate .haskell}
main :: IO ()
main = do
  o :: Opts <- getRecord "a random bit of text"
  let n = fromMaybe 10000 (runs o)
  let as = case sumTo o of
        [] -> [1,10,100,1000,10000]
        x -> x
  let trunc = fromMaybe 5 (truncAt o)
  let numChart = min (length as) $ fromMaybe 3 (chartNum o)
```

For reference, based on a 2.6G machine one cycle is = 0.38 𝛈s

`tick_` taps the register twice to get a sense of the cost.

``` {.sourceCode .literate .haskell}
  onetick <- tick_
  ticks <- replicateM 10 tick_
  avtick <- replicateM 1000000 tick_
  let average cs = L.fold ((/) <$> L.sum <*> L.genericLength) cs
  writeFile "other/onetick.md" $ code
    [ "one tick_: " <> pack (show onetick) <> " cycles"
    , "next 10: " <> pack (show ticks)
    , "average over 1m: " <>
      pack (show $ average (fromIntegral <$> avtick)) <> " cycles"
    ]
```

    one tick_: 20 cycles
    next 10: [14,12,48,12,14,14,14,48,12,14]
    average over 1m: 20.268154 cycles

Before we actually measure something, lets take a closer look at tick\_.

A pattern I see on my machine are shifts by multiples of 4, which
correspond to roughly the L1 [cache
latency](http://stackoverflow.com/questions/1126529/what-is-the-cost-of-an-l1-cache-miss).

It pays to look at the whole distribution, and a compact way of doing
that is to calculate quantiles:

``` {.sourceCode .literate .haskell}
  -- warmup 100
  xs <- replicateM 10000 tick_
  writeFile "other/tick_.md" $ code $
        (["[min, 10th, 20th, .. 90th, max]:"] :: [Text]) <>
        [mconcat (sformat (" " % prec 3) <$> deciles (fromIntegral <$> xs))]
```

    [min, 10th, 20th, .. 90th, max]:
     24.0 24.7 25.3 26.0 26.6 27.2 27.7 28.3 28.9 29.5 304

The important cycle count for most work is around the 30th to 50th
percentile, where you get a clean measure, hopefully free of GC activity
and cache miss-fires.

The quantile print of tick\_ sometimes shows a 12 to 14 point jump
around the 90th percential, and this is in the zone of an L2 access.
Without a warmup, one or more larger values occur at the start, and
often are in the zone of an L2 miss. Sometimes there's also a few large
hiccoughs at around 2k cycles.

summing
=======

Let's measure something. The simplest something I could think of was
summing.

The helper function `reportQuantiles` utilises `spins` which takes n
measurements of a function application over a range of values to apply.

``` {.sourceCode .literate .haskell}
  let f :: Double -> Double
      f x = foldl' (+) 0 [1..x]
  _ <- warmup 100
  (cs,_) <- spins n tick f as
  reportQuantiles cs as "other/spin.md"
```

     1.00e0:  560 582 584 586 589 592 595 599 605 646 3.30e5
     1.00e1:  174 176 178 180 200 211 215 247 248 254 5.46e4
     1.00e2:  122 122 123 124 126 126 126 127 149 170 4.98e3
     1.00e3:  119 120 123 127 135 145 165 166 195 208 616
     1.00e4:  119 124 128 132 135 139 145 157 181 196 272

Each row represents summing to a certain point: 1 up to 10000, and each
column is a decile: min, 10th .. 90th, max. The values in each cell are
the number of cycles divided by the number of runs and the number of
sumTo.

The first row (summing to 1) represents the cost of setting up the sum,
so we're looking at about (692 - 128) = 560 cycles every run.

Overall, the computation looks like it's running in O(n) where n is the
number of runs \* nuber of sumTo. Specifically, I would write down the
order at about:

    123 * o(n) + 560 * o(1)

The exception is the 70th to 90th zone, where the number of cycles
creeps up to 194 for 1k sumTo at the 90th percentile.

Charting the 1k sumTo:

``` {.sourceCode .literate .haskell}
  let xs_0 = fromIntegral <$> take 10000 (cs!!numChart) -- summing to 1000
  let xs1 = (\x -> min x (trunc*deciles xs_0 !! 5)) <$> xs_0
  fileSvg "other/spin1k.svg" (750,250) $ pad 1.1 $ histLine xs1
```

![](other/spin1k.svg)

Switching to a solo experiment gives:

    stack exec "ghc" -- -O2 -rtsopts examples/summing.lhs
    ./examples/summing +RTS -s -RTS --runs 10000 --sumTo 1000 --chart --chartName other/sum1e3.svg --truncAt 4

![](other/sum1e3.svg)

Executable complexity has changed the profile of the overall
computation. Both measurements have the same value, however, up to
around the 30th percentile.

generic vector
--------------

Using vector to sum:

``` {.sourceCode .literate .haskell}
  let asv :: [V.Vector Double] = (\x -> V.generate (floor x) fromIntegral) <$> as
  let sumv :: V.Vector Double -> Double
      sumv x = V.foldl (+) 0 x
  (csv,_) <- spins n tick sumv asv
  reportQuantiles csv as "other/vectorGen.md"
```

     1.00e0:  21.0 29.9 30.9 31.8 33.4 34.4 35.5 36.6 37.1 37.6 5.68e5
     1.00e1:  7.40 9.01 9.16 9.21 9.26 9.33 9.46 9.50 9.54 9.58 1.45e5
     1.00e2:  6.72 6.82 6.84 6.86 6.91 6.93 6.98 11.0 11.2 11.3 1.42e4
     1.00e3:  6.53 6.55 6.74 6.92 6.93 7.31 7.60 7.61 9.06 9.07 1.50e3
     1.00e4:  6.51 6.52 6.53 6.70 6.85 7.57 8.63 9.02 9.03 10.7 3.12e3

randomizing data
----------------

To avoid ghc (and fusion) trickiness, it's often a good idea to use
random numbers instead of simple progressions. One day, a compiler will
discover `x(x+1)/2` and then we'll be in real trouble.

``` {.sourceCode .literate .haskell}
  mwc <- create
  !asr <- (fmap V.fromList <$>) <$> sequence $ (\x -> samples x uniform mwc) . floor <$> as
  void $ warmup 100
  (csr,_) <- spins n tick sumv asr
  reportQuantiles csr as "other/vectorr.md"
```

     1.00e0:  21.0 30.1 30.8 31.6 32.6 33.6 36.3 40.1 43.1 44.3 1.01e6
     1.00e1:  7.40 8.42 9.85 9.98 10.0 10.1 10.1 10.1 10.4 10.5 1.48e5
     1.00e2:  6.76 6.81 6.83 6.84 6.85 6.87 6.90 6.92 6.94 6.96 1.31e4
     1.00e3:  6.53 6.54 6.63 6.66 6.67 6.69 6.74 6.84 6.88 7.07 6.59e3
     1.00e4:  8.62 8.68 8.80 8.81 8.83 8.86 8.91 9.08 10.2 12.2 196

Charting the 1k sumTo:

``` {.sourceCode .literate .haskell}
  let csr0 = fromIntegral <$> take 10000 (csr!!numChart) -- summing to 1000
  let csr1 = (\x -> min x (trunc*deciles csr0 !! 5)) <$> csr0
  fileSvg "other/vector1k.svg" (750,250) $ pad 1.1 $ histLine csr1
```

![](other/vector1k.svg)

unboxed
-------

``` {.sourceCode .literate .haskell}
  !asu <- (fmap U.fromList <$>) <$> sequence $ (\x -> samples x uniform mwc) . floor <$> as
  let sumu :: U.Vector Double -> Double
      sumu x = U.foldl (+) 0 x
  void $ warmup 100
  (csu,_) <- spins n tick sumu asu
  reportQuantiles csu as "other/vectoru.md"
```

     1.00e0:  12.0 27.2 27.5 27.8 28.1 28.4 28.7 29.1 30.7 36.2 1.04e6
     1.00e1:  3.20 4.75 4.92 4.99 5.07 5.13 5.19 5.26 5.33 5.44 1.00e5
     1.00e2:  2.38 2.55 2.57 2.59 2.60 2.61 2.63 2.64 2.66 2.72 1.05e4
     1.00e3:  2.19 2.20 2.20 2.21 2.24 2.29 2.33 2.34 2.34 3.60 1.68e3
     1.00e4:  2.17 2.17 2.17 2.17 2.23 2.52 2.52 3.01 3.01 3.01 105

Charting the 1k sumTo:

``` {.sourceCode .literate .haskell}
  let csu0 = fromIntegral <$> take 10000 (csu!!numChart) -- summing to 1000
  let csu1 = (\x -> min x (trunc*deciles csu0 !! 5)) <$> csu0
  fileSvg "other/vectoru1k.svg" (750,250) $ pad 1.1 $ histLine csu1
```

![](other/vectoru1k.svg)

storable
--------

``` {.sourceCode .literate .haskell}
  !ass <- (fmap S.fromList <$>) <$> sequence $ (\x -> samples x uniform mwc) . floor <$> as
  let sums :: S.Vector Double -> Double
      sums x = S.foldl (+) 0 x
  void $ warmup 100
  (css,_) <- spins n tick sums ass
  reportQuantiles css as "other/vectors.md"
```

     1.00e0:  12.0 25.4 25.9 26.4 26.9 27.4 27.8 28.3 28.7 30.8 1.06e6
     1.00e1:  3.00 3.57 3.69 3.82 4.26 4.38 4.47 4.55 4.62 4.69 1.01e5
     1.00e2:  2.38 2.47 2.53 2.54 2.56 2.57 2.71 2.74 2.76 2.79 1.04e4
     1.00e3:  2.19 2.20 2.20 2.20 2.20 2.31 2.40 3.20 3.60 3.60 948
     1.00e4:  2.17 2.17 2.17 2.23 2.30 2.52 2.58 3.01 3.01 3.55 111

Charting the 1k sumTo:

``` {.sourceCode .literate .haskell}
  let css0 = fromIntegral <$> take 10000 (css!!numChart) -- summing to 1000
  let css1 = (\x -> min x (trunc*deciles css0 !! 5)) <$> css0
  fileSvg "other/vectors1k.svg" (750,250) $ pad 1.1 $ histLine css1
```

![](other/vectors1k.svg)

tickf, ticka, tickfa
--------------------

These functions attempt to discriminate between cycles used to compute
`f a` (ie to apply the function f), and cycles used to force `a`. In
experiments so far, this act of observation tends to change the number
of cycles.

Separation of the `f` and `a` effects in `f a`

``` {.sourceCode .literate .haskell}
  void $ warmup 100
  (csr2,_) <- spins n tick sumv asr
  (csvf,_) <- spins n tickf sumv asr
  (csva,_) <- spins n ticka sumv asr
  (csvfa,_) <- spins n tickfa sumv asr
  reportQuantiles csr2 as "other/vectorr2.md"
  reportQuantiles csvf as "other/vectorf.md"
  reportQuantiles csva as "other/vectora.md"
  reportQuantiles (fmap fst <$> csvfa) as "other/vectorfaf.md"
  reportQuantiles (fmap snd <$> csvfa) as "other/vectorfaa.md"
```

a full tick

     1.00e0:  21.0 29.8 32.5 33.0 33.4 33.8 34.1 35.5 36.9 40.2 6.21e5
     1.00e1:  7.20 8.81 9.08 9.19 9.43 9.55 9.66 9.77 9.88 10.0 1.25e5
     1.00e2:  6.74 6.81 6.82 6.85 6.90 6.94 7.14 7.43 7.47 7.54 1.37e4
     1.00e3:  6.53 6.55 6.73 6.92 7.30 7.59 9.05 9.05 9.08 10.7 1.41e3
     1.00e4:  6.51 6.51 6.52 6.52 6.66 7.23 8.03 9.02 9.03 9.08 238

just function application

     1.00e0:  12.0 28.1 29.3 29.9 30.6 31.2 31.9 32.5 37.0 38.1 1.35e6
     1.00e1:  6.40 7.98 8.09 8.15 8.20 8.25 8.30 8.49 8.94 9.13 1.33e5
     1.00e2:  6.66 6.81 6.82 6.83 6.84 6.87 6.90 6.91 6.94 6.98 1.36e4
     1.00e3:  6.52 6.53 6.54 6.54 6.55 6.70 6.74 9.05 9.06 9.39 1.42e3
     1.00e4:  6.51 6.51 6.52 6.52 6.53 6.71 7.20 8.52 9.03 9.06 150

just forcing `a`:

     1.00e0:  12.0 12.9 13.4 13.8 14.2 14.6 15.7 21.7 24.7 47.5 306
     1.00e1:  1.20 1.42 1.76 2.05 2.07 2.09 2.12 2.14 2.44 2.61 7.60
     1.00e2:  0.120 0.129 0.136 0.143 0.150 0.208 0.214 0.219 0.224 0.229 0.740
     1.00e3:  0.0120 0.0136 0.0154 0.0198 0.0216 0.0219 0.0222 0.0225 0.0228 0.0280 0.200
     1.00e4:  0.00120 0.00525 0.00703 0.00759 0.00800 0.00836 0.00868 0.00913 0.00984 0.0109 0.0684

the f splice of f a

     1.00e0:  14.0 17.8 18.8 19.4 19.8 20.2 20.6 22.1 53.2 54.2 1.39e6
     1.00e1:  5.00 5.65 7.52 8.91 8.97 9.02 9.07 9.14 9.21 9.29 1.54e5
     1.00e2:  6.72 6.78 6.92 7.10 7.11 7.12 7.13 7.14 7.15 7.17 1.72e4
     1.00e3:  6.69 6.70 6.73 6.73 6.74 6.74 6.76 6.92 6.92 6.93 1.97e3
     1.00e4:  6.51 6.51 6.52 6.52 6.70 6.71 6.91 7.57 9.02 9.04 226

the a slice of fa

     1.00e0:  12.0 13.0 13.3 13.6 13.9 14.2 14.5 14.8 35.7 48.0 390
     1.00e1:  1.20 1.27 1.32 1.35 1.38 1.42 1.45 1.48 3.12 4.79 9.60
     1.00e2:  0.120 0.128 0.132 0.136 0.139 0.142 0.146 0.149 0.350 0.479 11.1
     1.00e3:  0.0120 0.0132 0.0137 0.0143 0.0148 0.0174 0.0204 0.0215 0.0229 0.0475 0.300
     1.00e4:  0.00120 0.00592 0.00679 0.00723 0.00757 0.00782 0.00819 0.00869 0.00957 0.0107 0.142

``` {.sourceCode .literate .haskell}
  pure ()
```

helpers
-------

``` {.sourceCode .literate .haskell}
showxs :: [Double] -> Double -> Text
showxs qs m =
          sformat (" " % Formatting.expt 2) m <> ": " <>
          mconcat (sformat (" " % prec 3) <$> ((/m) <$> qs))

deciles :: forall (f :: * -> *). Foldable f => f Double -> [Double]
deciles xs =
  (\x -> fromMaybe 0 $ quantile x (tdigest xs :: TDigest 25)) <$> ((0.1*) <$> [0..10])

reportQuantiles ::  [[Cycles]] -> [Double] -> FilePath -> IO ()
reportQuantiles css as name =
  writeFile name $ code $ zipWith showxs (deciles . fmap fromIntegral <$> css) as

code :: [Text] -> Text
code cs = "\n~~~\n" <> intercalate "\n" cs <> "\n~~~\n"

histLine :: [Double] -> Chart' a
histLine xs =
    line (repeat (LineConfig 0.002 (Color 0 0 1 0.1))) wideScreen
     (zipWith (\x y -> [V2 x 0,V2 x y]) [0..] xs) <>
        axes def wideScreen
        [ toCorners (V2
         (Range (0.0,fromIntegral $ length xs))
         (Range (0,L.fold (L.Fold max 0 identity) xs)))]
```

notes
=====

rdpmc
-----

A first-cousin of rdtsc,
[rdpmc](https://software.intel.com/en-us/forums/software-tuning-performance-optimization-platform-monitoring/topic/595214),
offers the possibility to track page faults, cache misses and other such
beasties, but lacks an easy entry-point c library.

workflow
--------

    stack build --copy-bins --exec "perf-examples" --exec "pandoc -f markdown+lhs -t html -i examples/examples.lhs -o index.html --filter pandoc-include" --exec "pandoc -f markdown+lhs -t markdown -i examples/examples.lhs -o readme.md --filter pandoc-include"

time performance references
---------------------------

[Optimising haskell for a tight inner
loop](http://neilmitchell.blogspot.co.uk/2014/01/optimising-haskell-for-tight-inner-loop.html)

[Tools for analysing
performance](http://stackoverflow.com/questions/3276240/tools-for-analyzing-performance-of-a-haskell-program/3276557#3276557)

[Write haskell as fast as
c](https://donsbot.wordpress.com/2008/05/06/write-haskell-as-fast-as-c-exploiting-strictness-laziness-and-recursion/)

[Reading ghc
core](http://stackoverflow.com/questions/6121146/reading-ghc-core)

space performance references
----------------------------

[Chasing space leaks in
shake](http://neilmitchell.blogspot.com.au/2013/02/chasing-space-leak-in-shake.html)

[Space leak zoo](http://blog.ezyang.com/2011/05/space-leak-zoo/)

[Anatomy of a thunk
leak](http://blog.ezyang.com/2011/05/anatomy-of-a-thunk-leak/)

[An insufficiently lazy
map](http://blog.ezyang.com/2011/05/an-insufficiently-lazy-map/)

[Pinpointing space leaks in big
programs](http://blog.ezyang.com/2011/06/pinpointing-space-leaks-in-big-programs/)

A (fairly old) checklist
------------------------

1.  compile with rtsopts flag

<!-- -->

    find . -name '*.o' -type f -print -delete
    find . -name '*.hl' -type f -print -delete
    ghc -O2 --make example/example.hs -fforce-recomp -isrc:example -rtsopts

2.  check GC `example +RTS -s`

3.  enabling profiling

-   a normal ghc
    `ghc -fforce-recomp --make -O2 -isrc example/example.hs`
-   profile enabled automatically
    `ghc -prof -auto -auto-all -fforce-recomp --make -O2 -isrc:dev A.hs`
-   if template haskell
    `ghc -osuf p_o -prof -auto -auto-all -fforce-recomp --make -O2 -isrc:dev A.hs`

4.  create an A.prof on execution: `time A +RTS -p`

5.  space

<!-- -->

      time dev/Reuters/A "test/data/reuters-100k.txt" +RTS -p -hc
      hp2ps -e8in -c A.hp

    hy = types
    hd = constructors

6.  strictness pragmas?

7.  space leaks

<!-- -->

    +RTS -s - additional memory
    +RTS -xt -hy

cache cycle estimates
---------------------

register | 4 per cycle |\
L1 Cache access| 3-4 cycles|\
L2 Cache access| 11-12 cycles|\
L3 unified access| 30 - 40|\
DRAM hit| 195 cycles|\
L1 miss | 40 cycles|\
L2 miss | &gt;600 cycles|
