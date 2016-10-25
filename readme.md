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
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.List
import Linear
import qualified Tower as T
import Data.Primitive.MutVar
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
    next 10: [37,22,22908,28,40,36,500,28,28,32]
    average over 1m: 22.679462 cycles

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

    [min, 10th, 20th, .. 90th, max]: 12.0 12.8 14.5 20.1 20.2 21.4 21.4 22.0 22.0 22.0 2.21e4

The important cycle count for most work is around the 30th to 50th
percentile, where you get a clean measure, hopefully free of cache
missing.

The quantile print of tick\_ often shows a 12 to 14 point jump around
the 70th to 90th percential, and this is probably a L2 miss, and then a
few large brain farts at around 2k cycles.

For reference, based on a 2.6G machine one cycle is = 0.38 𝛈s

spin
====

Let's measure something. The simplest something I could think of was
summing.

`spin` takes n measurements of whatever part you want to measure.

``` {.sourceCode .literate .haskell}
  _ <- warmup 100
  let f x = foldl' (+) 0 [1..x]
  let ms = [1, 10, 100, 1000, 10000, 100000]
  let n = 100
  res <- sequence $ (spin n tick f) <$> ms
  let xs = fmap fromIntegral <$> (fst <$> res) :: [[Double]]
  let qss = L.fold (quantiles' 11) <$> xs
  let showxs :: [Double] -> Double -> Text
      showxs qs m =
          (show m) <> ": " <>
          mconcat (sformat (" " % prec 3) <$> ((\x -> x/m) <$> qs))
  Text.writeFile "other/spin.md" $ code $
      zipWith showxs qss ms
```

    1.0:  420 438 442 442 444 460 468 472 692 706 5.19e4
    10.0:  157 160 160 160 161 165 172 175 180 180 215
    100.0:  130 131 133 135 137 138 140 143 182 196 9.45e3
    1000.0:  134 135 135 135 136 137 143 178 312 360 362
    10000.0:  179 186 188 191 192 195 197 200 203 210 223
    100000.0:  183 196 197 197 198 198 198 199 200 201 205

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

`spin n tickf f` takes n measures using the tickf version, which just
looks at function application effects.

``` {.sourceCode .literate .haskell}
  _ <- warmup 100
  let f x = foldl' (T.+) 0 [1..x]
  let ms = [1, 10, 100, 1000, 10000, 100000]
  let n = 100
  res <- sequence $ (spin n tickf f) <$> ms
  let xs = fmap fromIntegral <$> (fst <$> res) :: [[Double]]
  let qss = L.fold (quantiles' 11) <$> xs
  let showxs :: [Double] -> Double -> Text
      showxs qs m =
          (show m) <> ": " <>
          mconcat (sformat (" " % prec 3) <$> ((\x -> x/m) <$> qs))
  Text.writeFile "other/ticktower.md" $ code $
      zipWith showxs qss ms
```

    1.0:  447 451 455 456 459 459 496 523 593 823 7.92e3
    10.0:  164 166 166 166 167 175 180 183 184 191 195
    100.0:  135 136 137 137 137 138 140 158 193 473 2.18e3
    1000.0:  132 134 134 135 137 140 173 317 330 347 385
    10000.0:  171 183 185 185 185 186 191 192 195 203 209
    100000.0:  183 185 186 187 188 188 190 191 192 194 196

vector
------

Using vector to sum:

``` {.sourceCode .literate .haskell}
  _ <- warmup 100
  let f x = V.foldl (+) 0 $ V.replicate x 1
  let ms = [1, 10, 100, 1000, 10000, 100000]
  let n = 100
  res <- sequence $ (spin n tickf f) <$> ms
  let xs = fmap fromIntegral <$> (fst <$> res) :: [[Double]]
  let qss = L.fold (quantiles' 11) <$> xs
  let showxs :: [Double] -> Double -> Text
      showxs qs m =
          (show m) <> ": " <>
          mconcat (sformat (" " % prec 3) <$> ((\x -> x/m) <$> qs))
  Text.writeFile "other/vector1.md" $ code $
      zipWith showxs qss (fromIntegral <$> ms)
```

    1.0:  48.0 72.0 77.0 86.0 86.0 86.0 95.0 95.0 130 155 1.96e3
    10.0:  33.0 37.8 38.3 38.7 38.8 39.2 39.8 41.0 43.9 52.8 117
    100.0:  29.7 34.8 36.2 36.8 36.9 37.2 40.2 40.7 42.7 43.0 45.1
    1000.0:  29.6 29.7 29.8 29.8 29.8 29.9 29.9 30.6 35.8 54.8 284
    10000.0:  27.0 27.2 27.2 27.3 27.7 29.8 31.6 36.4 43.9 46.5 61.7
    100000.0:  30.9 31.5 31.7 31.9 32.1 32.2 32.3 32.7 32.7 33.0 34.6

unboxed vector
--------------

Using unboxed vector to sum Ints:

``` {.sourceCode .literate .haskell}
  _ <- warmup 100
  let f x = U.foldl' (+) (0::Int) $ U.replicate x 1
  let ms = [1, 10, 100, 1000, 10000, 100000]
  let n = 100
  res <- sequence $ (spin n tickf f) <$> ms
  let xs = fmap fromIntegral <$> (fst <$> res) :: [[Double]]
  let qss = L.fold (quantiles' 11) <$> xs
  let showxs :: [Double] -> Double -> Text
      showxs qs m =
          (show m) <> ": " <>
          mconcat (sformat (" " % prec 3) <$> ((\x -> x/m) <$> qs))
  Text.writeFile "other/vector2.md" $ code $
      zipWith showxs qss (fromIntegral <$> ms)
```

    1.0:  113 117 119 120 123 125 139 193 303 333 2.00e4
    10.0:  19.6 20.5 20.5 20.5 21.1 21.4 21.7 22.8 24.3 41.0 48.7
    100.0:  11.4 11.7 11.7 11.8 11.8 11.8 11.9 12.4 12.8 12.9 13.8
    1000.0:  10.6 11.1 11.1 11.2 11.2 11.3 11.3 11.9 14.3 47.4 536
    10000.0:  10.4 10.5 10.5 10.5 10.6 10.6 10.6 11.1 14.0 25.2 47.2
    100000.0:  11.0 13.1 13.2 13.3 13.5 13.5 13.6 13.7 13.8 13.8 14.4

Peak performance clocks in on my machine around 11 cycles per element.
Immutable is the important proviso.

``` {.sourceCode .literate .haskell}
  fileSvg "other/unboxed1k.svg" (300,300) $
      rect'
      def
      [ rectBorderColor .~ Color 0 0 0 0
      $ rectColor .~ Color 0.333 0.333 0.333 0.5
      $ def]
      [zipWith4 V4 [0..] (cycle [0]) [1..] (xs !! 3)]
```

Individual measurements for m=1000

![](other/unboxed1k.svg)

We are down to one 3e5 brain fart, but only because our computation is a
lot faster now. It's going to be something boring like GC, or an out of
order execution effect. Since it's regular, we can keep an eye on it,
and separate the effect whatever it is.

the `!f` and the `!a`
---------------------

Same unboxed Int vector, looking at `tickfa`:

``` {.sourceCode .literate .haskell}
  _ <- warmup 100
  let f x = U.foldl' (+) (0::Int) $ U.replicate x 1
  let ms = [1, 10, 100, 1000, 10000, 100000]
  let n = 100
  res <- sequence $ (spin n tickfa f) <$> ms
  res' <- sequence $ (spin n tick f) <$> ms
  let xsf = fmap (fromIntegral . fst) <$> (fst <$> res) :: [[Double]]
  let xsa = fmap (fromIntegral . snd) <$> (fst <$> res) :: [[Double]]
  let xsb = fmap (fromIntegral) <$> (fst <$> res') :: [[Double]]
  let qssf = L.fold (quantiles' 11) <$> xsf
  let qssa = L.fold (quantiles' 11) <$> xsa
  let qssb = L.fold (quantiles' 11) <$> xsb
  let showxs :: [Double] -> Double -> Text
      showxs qs m =
          (show m) <> ": " <>
          mconcat (sformat (" " % prec 3) <$> ((\x -> x/m) <$> qs))
  Text.writeFile "other/f.md" $ code $
      zipWith showxs qssf (fromIntegral <$> ms)
  Text.writeFile "other/a.md" $ code $
      zipWith showxs qssa (fromIntegral <$> ms)
  Text.writeFile "other/b.md" $ code $
      zipWith showxs qssb (fromIntegral <$> ms)
```

function application effect

    1.0:  119 122 122 122 125 125 125 128 129 324 3.36e3
    10.0:  18.6 19.3 19.4 19.6 19.6 19.8 20.3 21.9 24.7 27.6 39.5
    100.0:  11.1 11.3 11.4 11.5 11.9 13.2 15.5 16.8 25.1 312 4.81e3
    1000.0:  10.4 10.5 10.7 10.8 10.9 11.0 11.1 11.4 11.6 19.9 342
    10000.0:  10.2 10.3 10.5 10.5 10.5 10.5 10.6 10.8 11.2 26.2 36.1
    100000.0:  11.1 12.9 13.0 13.1 13.2 13.3 13.4 13.5 13.6 13.8 14.2

instantiation effect

    1.0:  18.0 18.2 21.0 21.0 21.1 21.4 22.1 22.1 22.6 24.0 107
    10.0:  1.80 1.81 2.00 2.10 2.10 2.10 2.10 2.12 2.20 2.22 3.40
    100.0:  0.180 0.197 0.210 0.210 0.211 0.224 0.229 0.251 0.277 0.313 0.580
    1000.0:  0.0120 0.0140 0.0180 0.0180 0.0180 0.0210 0.0210 0.0230 0.0267 0.0520 0.0950
    10000.0:  0.00140 0.00258 0.00437 0.00438 0.00480 0.00525 0.00571 0.00583 0.00640 0.00683 0.00890
    100000.0:  0.000180 0.000220 0.000400 0.000403 0.000468 0.000550 0.000552 0.000573 0.000673 0.000777 0.00100

both effects

    1.0:  156 192 214 217 232 239 264 285 434 630 1.51e5
    10.0:  19.3 27.9 28.3 28.9 29.2 29.8 30.4 31.5 34.1 42.3 70.7
    100.0:  11.2 11.4 11.4 11.5 11.7 11.8 11.9 12.0 12.4 13.4 16.2
    1000.0:  10.5 10.7 10.7 10.8 10.8 10.9 10.9 10.9 11.0 11.7 249
    10000.0:  10.4 10.5 10.5 10.5 10.5 10.5 10.5 10.8 12.2 29.8 37.1
    100000.0:  10.7 12.9 13.1 13.2 13.3 13.4 13.4 13.6 13.8 14.0 14.2

mutation
--------

Mutable summer of Ints:

``` {.sourceCode .literate .haskell}
  _ <- warmup 100
  let f x = L.foldM mutFold $ V.replicate x (1::Int)
  let ms = [1, 10, 100, 1000, 10000, 100000]
  let n = 100
  res <- sequence $ (spinM n tickfM f) <$> ms
  let xs = fmap fromIntegral <$> (fst <$> res) :: [[Double]]
  let xsres = snd <$> res :: [Int]
  print $ L.fold (L.Fold (+) 0 Protolude.identity) xsres
  let qss = L.fold (quantiles' 11) <$> xs
  let showxs :: [Double] -> Double -> Text
      showxs qs m =
          (show m) <> ": " <>
          mconcat (sformat (" " % prec 3) <$> ((\x -> x/m) <$> qs))
  Text.writeFile "other/mutable.md" $ code $
      zipWith showxs qss (fromIntegral <$> ms)
```

    1.0:  45.0 65.0 65.1 77.0 86.0 86.0 95.0 104 107 190 850
    10.0:  19.3 28.5 31.5 35.7 36.3 36.6 38.3 38.4 43.4 48.7 63.3
    100.0:  11.3 17.7 19.3 21.3 21.5 22.2 22.6 23.1 28.5 30.6 32.3
    1000.0:  11.1 11.7 11.9 12.1 13.0 15.4 18.9 21.2 26.6 106 395
    10000.0:  9.47 9.99 10.2 11.3 11.6 12.2 14.1 28.2 32.5 36.2 37.0
    100000.0:  14.0 15.0 15.3 15.3 15.5 15.6 15.8 16.1 16.1 16.2 17.6

helpers
-------

``` {.sourceCode .literate .haskell}

code cs = mconcat $ (<> "\n") <$> ("    " <>) <$> cs
```

``` {.sourceCode .literate .haskell}
mutFold :: L.FoldM IO Int Int
mutFold = L.FoldM step begin done
  where
    step x a = modifyMutVar' x (+a) >> pure x
    begin = newMutVar 0
    done = readMutVar
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
