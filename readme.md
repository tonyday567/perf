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
{-# LANGUAGE DataKinds #-}
import Data.Primitive.MutVar
import Data.Text (pack)
import Data.Text.IO (writeFile)
import Formatting
import Protolude hiding ((%))
import Data.List.NonEmpty (NonEmpty(..))

import qualified Control.Foldl as L
import qualified Data.Vector as V

import Perf.Cycles
import Data.TDigest
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

    one tick_: 100 cycles
    next 10: [32,28,24,28,28,24,28,24,28,28]
    average over 1m: 20.868261 cycles

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
  let qs = (\x -> quantile x (tdigest xs :: TDigest 25)) <$> ((0.1*) <$> ([0..10]))
  writeFile "other/quantiles.md" $
        "\n    [min, 10th, 20th, .. 90th, max]:" <>
        mconcat (sformat (" " % prec 3) <$> (fromMaybe 0 <$> qs))
```

    [min, 10th, 20th, .. 90th, max]: 12.0 17.4 19.2 20.2 20.6 20.9 21.3 21.7 22.2 22.7 960

The important cycle count for most work is around the 30th to 50th
percentile, where you get a clean measure, hopefully free of cache
missing.

The quantile print of tick\_ often shows a 12 to 14 point jump around
the 70th to 90th percential, and this is probably a L2 miss, and then a
few large hiccoughs at around 2k cycles.

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

     1.00e0:  486 497 499 502 503 504 505 506 511 541 3.85e3
     1.00e1:  162 164 165 165 165 167 169 170 172 177 189
     1.00e2:  130 130 130 130 131 131 131 131 132 165 2.98e3
     1.00e3:  127 127 127 127 127 128 139 325 386 409 524
     1.00e4:  185 196 200 204 207 211 215 217 223 232 260
     1.00e5:  174 177 178 178 179 179 180 182 200 205 210

vector
------

Using vector to sum:

``` {.sourceCode .literate .haskell}
  let f :: Double -> Double
      f x = V.foldl (+) 0 $ V.replicate (floor x) 1
  _ <- runTick f tickf ms n "other/vector1.md"
```

     1.00e0:  33.0 33.4 34.1 34.8 35.8 36.6 36.9 37.3 37.6 37.9 881
     1.00e1:  6.40 6.90 6.98 7.06 7.13 7.19 7.27 7.35 7.46 7.69 14.1
     1.00e2:  7.86 7.89 7.91 7.92 7.92 7.93 7.94 7.95 7.96 7.97 8.02
     1.00e3:  7.67 7.67 7.67 7.67 7.67 7.68 7.68 7.68 7.68 7.68 7.70
     1.00e4:  7.22 7.22 7.22 7.22 7.23 7.23 7.23 7.23 7.23 7.23 9.75
     1.00e5:  7.22 7.22 7.22 7.38 7.58 8.39 8.39 9.80 10.0 10.7 12.4

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
  let qss = (\xs -> (\x -> fromMaybe 0 $ quantile x (tdigest xs :: TDigest 25)) <$> ((0.1*) <$> ([0..10]))) <$> xs
  let showxs :: [Double] -> Double -> Text
      showxs qs m =
          sformat (" " % Formatting.expt 2) m <> ": " <>
          mconcat (sformat (" " % prec 3) <$> ((/m) <$> qs))
  writeFile "other/mutable.md" $ code $
      zipWith showxs qss (fromIntegral <$> ms)
```

     1.00e0:  22.0 27.8 28.9 33.0 53.0 57.3 61.7 63.5 64.1 64.7 1.05e3
     1.00e1:  12.0 12.0 12.1 12.1 12.2 12.2 12.3 14.0 15.5 16.7 21.0
     1.00e2:  9.88 10.2 10.3 10.4 10.7 11.2 15.5 16.8 17.9 19.3 3.26e3
     1.00e3:  10.1 10.2 10.2 10.2 10.3 10.3 10.5 10.7 11.5 16.8 223
     1.00e4:  10.1 10.2 10.2 10.2 10.3 10.4 10.9 19.0 26.9 29.8 33.0
     1.00e5:  14.3 15.0 15.3 15.5 15.6 15.7 15.9 16.0 16.1 16.3 17.7

helpers
-------

``` {.sourceCode .literate .haskell}
runTick f t ms n name = do
    _ <- warmup 100
    res <- sequence $ spin n t f <$> ms
    let xs = fmap fromIntegral <$> (fst <$> res) :: [[Double]]
    let qss = (\xs -> (\x -> fromMaybe 0 $ quantile x (tdigest xs :: TDigest 25)) <$> ((0.1*) <$> ([0..10]))) <$> xs
    writeFile name $ code $ zipWith showxs qss ms
    return (qss, xs)
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
