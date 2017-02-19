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

``` {.sourceCode .literate .haskell}
{-# LANGUAGE OverloadedStrings #-}
import Data.Primitive.MutVar
import Data.Text (pack)
import Data.Text.IO (writeFile)
import Formatting
import Protolude hiding ((%))

import qualified Control.Foldl as L
import qualified Data.Vector as V

import Perf.Cycles
import Perf.Quantiles
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
  writeFile "other/onetick.md" $ code
    [ "one tick_: " <> pack (show onetick) <> " cycles"
    , "next 10: " <> pack (show ticks)
    , "average over 1m: " <>
      pack (show $ average (fromIntegral <$> avtick)) <> " cycles"
    ]
```

    one tick_: 40 cycles
    next 10: [25,22,22,24,21,18,18,19,22,21]
    average over 1m: 21.571953 cycles

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

    [min, 10th, 20th, .. 90th, max]: 24.0 28.0 28.0 28.0 32.0 32.9 35.1 35.1 36.1 37.5 340

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
  let f :: Double -> Double
      f x = foldl' (+) 0 [1..x]
  let ms = [1, 10, 100, 1000, 10000, 100000]
  let n = 100
  (xs, _) <- runTick f tick ms n "other/spin.md"
```

     1.00e0:  418 424 433 434 436 446 454 598 642 654 4.41e4
     1.00e1:  150 151 151 152 153 156 161 172 177 179 218
     1.00e2:  122 125 126 129 134 141 164 270 652 1.15e3 6.24e3
     1.00e3:  127 127 127 128 129 133 160 309 351 358 360
     1.00e4:  167 174 176 179 181 183 186 188 193 198 203
     1.00e5:  176 178 179 179 180 180 181 182 184 189 198

vector
------

Using vector to sum:

``` {.sourceCode .literate .haskell}
  let f :: Double -> Double
      f x = V.foldl (+) 0 $ V.replicate (floor x) 1
  _ <- runTick f tickf ms n "other/vector1.md"
```

     1.00e0:  33.0 37.0 37.0 37.9 40.0 40.0 40.0 40.3 43.0 91.0 1.55e3
     1.00e1:  6.70 7.30 7.30 7.30 7.31 7.36 7.45 7.50 8.00 11.0 14.1
     1.00e2:  7.89 7.92 7.92 7.92 7.92 7.93 7.95 7.95 7.96 7.98 8.41
     1.00e3:  7.43 7.68 7.68 7.68 7.68 7.68 7.68 7.70 7.75 10.4 30.2
     1.00e4:  7.22 7.23 7.43 7.43 7.43 7.43 7.43 7.43 7.43 7.43 9.65
     1.00e5:  7.22 7.23 7.32 7.65 7.65 7.91 8.67 10.0 10.1 11.8 14.1

mutation
--------

Mutable summer of Doubles:

``` {.sourceCode .literate .haskell}
  _ <- warmup 100
  let f x = L.foldM mutFold $ V.replicate x (1::Int)
  let ms = [1, 10, 100, 1000, 10000, 100000]
  let n = 100
  res <- sequence $ spinM n tickfM f <$> ms
  let xs = fmap fromIntegral <$> (fst <$> res) :: [[Double]]
  let xsres = snd <$> res :: [Int]
  print $ L.fold (L.Fold (+) 0 Protolude.identity) xsres
  let qss = L.fold (quantiles' 11) <$> xs
  let showxs :: [Double] -> Double -> Text
      showxs qs m =
          sformat (" " % Formatting.expt 2) m <> ": " <>
          mconcat (sformat (" " % prec 3) <$> ((/m) <$> qs))
  writeFile "other/mutable.md" $ code $
      zipWith showxs qss (fromIntegral <$> ms)
```

     1.00e0:  43.0 46.0 46.0 46.0 46.2 48.0 49.0 49.1 52.0 101 835
     1.00e1:  12.8 13.5 13.5 13.7 13.7 13.8 13.9 14.7 17.7 18.1 34.9
     1.00e2:  10.5 10.5 10.7 10.7 10.7 11.0 11.2 11.3 12.3 13.3 13.6
     1.00e3:  10.2 10.3 10.3 10.4 10.6 11.3 13.4 20.6 65.0 117 650
     1.00e4:  10.2 10.3 10.4 10.4 10.4 10.6 11.2 27.5 28.1 28.5 33.1
     1.00e5:  14.4 15.3 15.4 15.6 15.8 16.0 16.5 16.5 16.6 16.8 18.3

helpers
-------

``` {.sourceCode .literate .haskell}
runTick f t ms n name = do
    _ <- warmup 100
    res <- sequence $ spin n t f <$> ms
    let xs = fmap fromIntegral <$> (fst <$> res) :: [[Double]]
    let qs = L.fold (quantiles' 11) <$> xs
    writeFile name $ code $ zipWith showxs qs ms
    return (qs, xs)
  where
      showxs :: [Double] -> Double -> Text
      showxs qs m =
          sformat (" " % Formatting.expt 2) m <> ": " <>
          mconcat (sformat (" " % prec 3) <$> ((/m) <$> qs))

code cs = mconcat $ (<> "\n") . ("    " <>) <$> cs
```

``` {.sourceCode .literate .haskell}
mutFold :: L.FoldM IO Int Int
mutFold = L.FoldM step begin done
  where
    step x a = modifyMutVar' x (+a) >> pure x
    begin = newMutVar 0
    done = readMutVar
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

    stack install && perf-examples && pandoc -f markdown+lhs -t html -i examples/examples.lhs -o index.html --filter pandoc-include && pandoc -f markdown+lhs -t markdown -i examples/examples.lhs -o readme.md --filter pandoc-include

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
