<meta charset="utf-8"> <link rel="stylesheet" href="other/lhs.css">
<script type="text/javascript" async
  src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML">
</script>
[perf](https://tonyday567.github.io/perf/index.html) [![Build Status](https://travis-ci.org/tonyday567/perf.png)](https://travis-ci.org/tonyday567/perf)
========================================================================================================================================================

If you want to make stuff very fast in haskell, you need to dig down
below the criterion abstraction-level and start counting cycles using
the [rdtsc](https://en.wikipedia.org/wiki/Time_Stamp_Counter) register
on x86.

``` {.sourceCode .literate .haskell}
{-# LANGUAGE OverloadedStrings #-}
import Protolude hiding ((%))
import qualified Data.Text.IO as Text
import qualified Data.Text as Text
import Formatting
import Online
import qualified Control.Foldl as L
import Math.Combinatorics.Exact.Primes
import Perf.Cycles
import Chart.Unit hiding ((<>))
import Chart.Types
import Control.Lens
import Data.Default
import Data.List
import Linear
import qualified Tower as T
```

main
----

``` {.sourceCode .literate .haskell}
main :: IO ()
main = do
```

`tick_` taps the register twice to get a sense of the cost.

``` {.sourceCode .literate .haskell}
  onetick <- tick_
  ticks <- replicateM 10 tick_
  avtick <- replicateM 1000000 tick_
  let average cs = L.fold ((/) <$> L.sum <*> L.genericLength) cs
  Text.writeFile "other/onetick.md" $ code
    [ "one tick_: " <> (Text.pack $ show onetick) <> " cycles"
    , "next 10: " <> (Text.pack $ show ticks)
    , "average over 1m: " <>
      (Text.pack $ show $ average (fromIntegral <$> avtick)) <> " cycles"
    ]

```

    one tick_: 46 cycles
    next 10: [25,21,22,22,21,18,21,22,25,21]
    average over 1m: 20.571691 cycles

It often makes sense to give tick\_ a few spins before measuring
something, to warm everything up.

A pattern I see on my machine are shifts by multiples of 4, which
correspond to roughly the L1 [cache
latency](http://stackoverflow.com/questions/1126529/what-is-the-cost-of-an-l1-cache-miss).

It pays to look at the whole distribution, and a compact way of doing
that is to calculate quantiles:

``` {.sourceCode .literate .haskell}
  _ <- warmup 100
  xs' <- replicateM 10000 tick_
  let xs = fromIntegral <$> xs' :: [Double]
  let qs = L.fold (quantiles' 11) xs
  writeFile "other/quantiles.md" $
        "\n    [min, 10th, 20th, .. 90th, max]:" <>
        mconcat (sformat (" " % prec 3) <$> qs)
```

    [min, 10th, 20th, .. 90th, max]: 12.0 14.2 15.7 20.0 20.2 22.0 22.0 22.0 22.1 22.3 238

The important cycle count for most work is around the 30th to 50th
percentile, where you get a clean measure, hopefully free of cache
missing.

The quantile print of tick\_ often shows a 12 to 14 point jump around
the 70th to 90th percential, and this is probably a L2 miss, and then a
few large brain farts at around 2k cycles.

For reference, based on a 2.6G machine one cycle is = 0.38 𝛈s

tickn
=====

Let's measure something. The simplest something I could think of was
summing.

`tickn` takes n measurements

``` {.sourceCode .literate .haskell}
  _ <- warmup 100
  let f x = foldl' (+) 0 [1..x]
  let ms = [1, 10, 100, 1000, 10000, 100000]
  let n = 100
  res <- sequence $ (tickn n f) <$> ms
  let xs = fmap fromIntegral <$> (fst <$> res) :: [[Double]]
  let qss = L.fold (quantiles' 11) <$> xs
  let showxs :: [Double] -> Double -> Text
      showxs qs m =
          (show m) <> ": " <>
          mconcat (sformat (" " % prec 3) <$> ((\x -> x/m) <$> qs))
  Text.writeFile "other/tickn.md" $ code $
      zipWith showxs qss ms
```

    1.0:  482 488 489 520 544 665 746 755 871 1.38e3 5.15e4
    10.0:  177 178 178 180 195 195 202 204 205 207 230
    100.0:  125 133 133 134 137 164 165 193 205 239 1.93e4
    1000.0:  129 130 130 130 132 137 148 195 300 326 362
    10000.0:  170 174 177 179 181 183 185 186 190 193 203
    100000.0:  177 179 180 181 181 182 182 182 183 184 188

time series
-----------

``` {.sourceCode .literate .haskell}
  fileSvg "other/raw1k.svg" (300,300) $
      rect'
      def
      [ rectBorderColor .~ Color 0 0 0 0
      $ rectColor .~ Color 0.333 0.333 0.333 0.5
      $ def]
      [zipWith4 V4 [0..] (cycle [0]) [1..] (xs !! 3)]
      
  fileSvg "other/raw100.svg" (300,300) $
      rect'
      def
      [ rectBorderColor .~ Color 0 0 0 0
      $ rectColor .~ Color 0.333 0.333 0.333 0.5
      $ def]
      [zipWith4 V4 [0..] (cycle [0]) [1..] (xs !! 2)]
```

Individual measurements for m=100

![](other/raw100.svg)

Individual measurements for m=1000

![](other/raw1k.svg)

On my run, a 3e5 cycle process comes along every 3e6 or so and smashes
the loop.

Tower
-----

``` {.sourceCode .literate .haskell}
  _ <- warmup 100
  let f x = foldl' (T.+) 0 [1..x]
  let ms = [1, 10, 100, 1000, 10000, 100000]
  let n = 100
  res <- sequence $ (tickn n f) <$> ms
  let xs = fmap fromIntegral <$> (fst <$> res) :: [[Double]]
  let qss = L.fold (quantiles' 11) <$> xs
  let showxs :: [Double] -> Double -> Text
      showxs qs m =
          (show m) <> ": " <>
          mconcat (sformat (" " % prec 3) <$> ((\x -> x/m) <$> qs))
  Text.writeFile "other/ticktower.md" $ code $
      zipWith showxs qss ms
```

    1.0:  443 450 455 456 456 462 490 563 627 902 6.26e3
    10.0:  161 163 163 166 169 169 172 174 181 181 197
    100.0:  132 133 133 133 133 135 141 167 184 224 3.03e3
    1000.0:  129 130 130 130 131 133 156 276 292 312 354
    10000.0:  164 177 177 179 180 182 184 185 188 195 206
    100000.0:  175 179 179 181 181 181 182 182 183 184 187

helpers
-------

``` {.sourceCode .literate .haskell}

code cs = mconcat $ (<> "\n") <$> ("    " <>) <$> cs
```

rdpmc
-----

A first-cousin of rdtsc,
[rdpmc](https://software.intel.com/en-us/forums/software-tuning-performance-optimization-platform-monitoring/topic/595214),
offers the possibility to track page faults, cache misses and other such
beasties, but lacks an easy entry-point c library.

workflow
--------

    stack install && readme && pandoc -f markdown+lhs -t html -i readme.lhs -o index.html --filter pandoc-include
