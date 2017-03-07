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
import Chart
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

    one tick_: 30 cycles
    next 10: [21,21,18,22,22,21,21,18,22,22]
    average over 1m: 20.784972 cycles

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
     12.0 13.0 14.2 18.4 19.9 21.1 21.5 21.9 22.3 22.7 680

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

     1.00e0:  414 428 430 432 434 436 438 461 466 479 2.38e5
     1.00e1:  149 151 151 152 154 155 156 158 164 250 4.35e4
     1.00e2:  122 125 126 130 130 142 143 169 170 200 5.82e3
     1.00e3:  119 119 122 123 133 147 163 165 182 196 585
     1.00e4:  119 124 127 130 133 135 141 150 175 186 276

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

     1.00e0:  21.0 29.6 30.9 32.0 32.9 33.9 36.5 41.0 43.1 44.5 4.11e5
     1.00e1:  7.80 9.49 9.69 9.78 9.87 9.95 10.0 10.1 10.2 10.3 1.55e5
     1.00e2:  6.70 6.79 7.01 7.11 7.12 7.13 7.14 7.17 7.27 7.98 1.49e4
     1.00e3:  6.52 6.55 6.70 6.74 6.74 6.78 7.49 7.60 9.00 9.16 2.32e3
     1.00e4:  6.51 6.54 6.57 6.73 7.02 7.60 8.89 9.07 9.08 10.7 210

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

     1.00e0:  14.0 21.0 22.9 27.0 28.3 29.3 30.0 31.8 37.7 50.5 1.16e6
     1.00e1:  4.60 5.80 8.83 9.00 9.10 9.15 9.21 9.26 9.34 9.47 1.48e5
     1.00e2:  6.72 6.80 7.00 7.12 7.13 7.14 7.14 7.15 7.17 7.19 1.36e4
     1.00e3:  6.54 6.61 7.62 7.63 8.37 9.09 9.10 9.11 9.13 10.8 1.78e3
     1.00e4:  6.58 6.61 6.63 6.80 7.00 7.67 9.14 9.15 9.17 10.8 230

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

     1.00e0:  28.0 38.1 38.9 39.8 40.6 41.5 42.4 43.4 44.4 45.5 1.26e6
     1.00e1:  4.40 6.28 6.37 6.46 6.55 6.71 6.93 7.10 7.25 7.46 1.47e5
     1.00e2:  2.74 3.18 3.46 3.47 3.48 3.49 3.50 3.52 3.55 3.56 1.34e4
     1.00e3:  2.19 2.20 2.20 2.21 2.24 2.27 2.27 2.55 2.56 2.56 1.09e3
     1.00e4:  2.17 2.17 2.17 2.23 2.28 2.52 2.55 3.01 3.01 3.55 142

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

     1.00e0:  24.0 34.5 36.0 37.5 38.5 39.3 40.0 40.8 41.5 43.3 1.28e6
     1.00e1:  4.00 5.51 5.63 5.76 5.88 6.00 6.12 6.26 6.42 6.58 181
     1.00e2:  2.65 2.87 2.91 3.11 3.40 3.44 3.45 3.48 3.50 3.53 1.32e4
     1.00e3:  2.19 2.25 2.27 2.27 2.30 2.55 2.55 2.56 2.56 2.56 1.12e3
     1.00e4:  2.17 2.17 2.17 2.23 2.33 2.52 3.01 3.01 3.01 3.55 128

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

     1.00e0:  18.0 29.2 30.1 30.9 33.0 33.7 34.3 39.4 43.0 43.9 9.08e5
     1.00e1:  6.80 8.03 8.21 8.27 8.54 9.06 9.50 9.75 9.83 10.1 1.35e5
     1.00e2:  6.72 6.79 6.86 7.11 7.12 7.13 7.14 7.14 7.17 7.20 1.48e4
     1.00e3:  6.53 6.55 6.72 6.80 6.93 7.59 8.53 9.05 9.06 10.7 1.51e3
     1.00e4:  6.51 6.52 6.62 6.71 7.28 7.57 9.02 9.02 9.04 10.7 155

just function application

     1.00e0:  24.0 38.6 39.4 40.2 40.9 41.7 42.6 43.6 44.5 45.5 1.89e6
     1.00e1:  9.60 11.4 11.5 11.5 11.6 11.7 11.7 11.8 11.9 12.1 1.88e5
     1.00e2:  9.28 9.43 9.45 9.46 9.48 9.49 9.51 9.52 9.54 9.58 1.85e4
     1.00e3:  6.53 6.64 6.74 7.59 7.59 7.60 9.05 9.05 9.06 10.2 1.85e3
     1.00e4:  6.51 6.52 6.70 6.70 6.85 7.56 7.57 9.02 9.03 10.7 151

just forcing `a`:

     1.00e0:  12.0 12.5 13.0 13.4 13.7 14.1 14.4 14.7 23.8 47.8 88.0
     1.00e1:  1.20 1.29 1.33 1.36 1.40 1.43 1.47 1.51 3.62 4.81 11.8
     1.00e2:  0.120 0.135 0.152 0.196 0.216 0.220 0.223 0.227 0.237 0.346 1.40
     1.00e3:  0.0120 0.0207 0.0228 0.0252 0.0277 0.0279 0.0281 0.0283 0.0301 0.0357 0.172
     1.00e4:  0.00120 0.00384 0.00496 0.00539 0.00586 0.00647 0.00689 0.00731 0.00789 0.00877 0.0800

the f splice of f a

     1.00e0:  28.0 35.3 37.2 38.3 38.9 39.5 40.1 40.7 41.4 42.0 1.83e6
     1.00e1:  7.80 9.22 9.43 9.54 9.66 9.79 9.92 11.1 11.2 11.7 2.26e5
     1.00e2:  9.36 9.45 9.49 9.52 9.57 11.1 11.1 11.2 11.2 11.3 2.19e4
     1.00e3:  6.53 6.54 6.55 6.73 7.58 7.59 8.67 9.05 9.06 10.7 2.09e3
     1.00e4:  6.51 6.52 6.59 6.72 7.57 7.58 9.03 9.03 9.04 10.7 205

the a slice of fa

     1.00e0:  24.0 25.5 26.3 26.8 27.3 27.8 28.3 28.8 29.3 29.8 152
     1.00e1:  1.80 1.98 2.06 2.13 2.21 2.40 2.77 2.81 2.85 3.17 42.8
     1.00e2:  0.240 0.260 0.268 0.275 0.283 0.290 0.298 0.342 0.356 0.370 2.32
     1.00e3:  0.0120 0.0142 0.0201 0.0213 0.0223 0.0241 0.0278 0.0284 0.0313 0.0363 0.176
     1.00e4:  0.00140 0.00396 0.00484 0.00525 0.00571 0.00621 0.00666 0.00716 0.00759 0.00845 0.0422

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
    lines (repeat (LineConfig 0.002 (Color 0 0 1 0.1))) widescreen
     (zipWith (\x y -> [V2 x 0,V2 x y]) [0..] xs) <>
     axes
     ( chartAspect .~ widescreen
     $ chartRange .~ Just
       (V2
         (Range (0.0,fromIntegral $ length xs))
         (Range (0,L.fold (L.Fold max 0 identity) xs)))
     $ def)
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
