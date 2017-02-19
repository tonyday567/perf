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
import Protolude hiding ((%))
import qualified Data.Text.IO as Text
import qualified Data.Text as Text
import Formatting
import qualified Control.Foldl as L
import Math.Combinatorics.Exact.Primes
import Perf.Cycles
import Perf.Quantiles
import Control.Lens
import Data.Default
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.List
import Linear
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
    [ "one tick_: " <> Text.pack (show onetick) <> " cycles"
    , "next 10: " <> Text.pack (show ticks)
    , "average over 1m: " <>
      Text.pack (show $ average (fromIntegral <$> avtick)) <> " cycles"
    ]

```

    one tick_: 42 cycles
    next 10: [24,32,18,21,18,21,21,18,32,21]
    average over 1m: 20.330693 cycles

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

    [min, 10th, 20th, .. 90th, max]: 12.0 20.7 22.0 22.0 22.0 22.0 22.0 22.1 22.7 24.0 680

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

     1.0e0:  449 454 457 467 471 481 558 679 786 4.84e3 4.74e4
     1.0e1:  164 166 167 168 171 188 195 201 223 225 320
     1.0e2:  127 136 136 137 138 138 139 158 171 217 3.55e3
     1.0e3:  131 132 132 133 135 148 165 254 330 352 405
     1.0e4:  176 183 184 187 189 189 193 196 202 210 222
     1.0e5:  181 188 189 190 191 197 205 212 213 214 216

vector
------

Using vector to sum:

``` {.sourceCode .literate .haskell}
  let f :: Double -> Double
      f x = V.foldl (+) 0 $ V.replicate (floor x) 1
  _ <- runTick f tickf ms n "other/vector1.md"
```

     1.0e0:  33.0 35.2 36.4 37.0 37.0 37.0 37.3 40.0 40.0 94.6 1.13e3
     1.0e1:  7.00 7.30 7.35 7.46 7.52 7.61 7.63 7.77 11.6 11.9 13.8
     1.0e2:  7.89 7.92 7.92 7.92 7.95 7.95 7.95 7.96 7.96 8.08 10.5
     1.0e3:  7.43 7.68 7.68 7.68 7.75 7.75 7.76 7.76 7.83 7.96 30.3
     1.0e4:  7.43 7.43 7.43 7.43 7.43 7.43 7.43 7.44 7.47 7.92 9.80
     1.0e5:  7.43 7.43 7.43 7.44 7.54 7.65 7.68 7.90 8.59 10.1 12.4

unboxed vector
--------------

Using unboxed vector to sum Ints:

``` {.sourceCode .literate .haskell}
  let f :: Double -> Double
      f x = U.foldl' (+) (0::Double) $ U.replicate (floor x) 1
  (xs, _) <- runTick f tickf ms n "other/vector2.md"
```

     1.0e0:  122 125 125 125 125 127 129 156 251 471 3.51e4
     1.0e1:  19.3 20.2 20.2 20.2 20.3 20.7 21.3 22.6 24.5 28.1 50.7
     1.0e2:  10.5 10.6 10.6 10.6 10.7 10.8 11.0 11.2 11.4 12.7 18.0
     1.0e3:  9.39 9.74 9.81 9.85 9.85 9.92 9.94 14.3 14.5 15.2 883
     1.0e4:  9.38 9.45 9.47 9.47 9.48 9.50 9.51 9.55 9.82 22.2 35.9
     1.0e5:  9.77 11.3 11.4 11.5 11.5 11.5 11.6 11.9 12.1 12.2 13.0

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
          show m <> ": " <>
          mconcat (sformat (" " % prec 3) <$> ((/m) <$> qs))
  Text.writeFile "other/mutable.md" $ code $
      zipWith showxs qss (fromIntegral <$> ms)
```

    1.0:  43.0 49.0 49.0 49.0 49.0 49.0 49.2 50.0 55.0 92.0 1.16e3
    10.0:  11.4 13.1 13.1 13.1 13.1 13.3 14.6 35.1 165 753 4.33e3
    100.0:  9.44 9.58 9.60 9.72 9.98 10.7 11.9 16.9 29.1 339 1.18e4
    1000.0:  9.19 9.56 9.81 10.9 10.9 11.5 12.0 13.4 26.6 250 2.70e3
    10000.0:  9.46 9.70 9.81 9.84 10.1 10.8 14.5 18.1 29.7 33.0 42.2
    100000.0:  14.8 15.8 16.0 16.2 16.2 16.3 16.6 16.6 16.7 17.6 20.7

I had to rewrite the code to actually use the eventual values, or else
it's a noop in IO.

No major difference between mutability and immutablility. Something else
is happening...

helpers
-------

``` {.sourceCode .literate .haskell}


runTick f t ms n name = do
    _ <- warmup 100
    res <- sequence $ spin n t f <$> ms
    let xs = fmap fromIntegral <$> (fst <$> res) :: [[Double]]
    let qs = L.fold (quantiles' 11) <$> xs
    Text.writeFile name $ code $ zipWith showxs qs ms
    return (qs, xs)
  where
      showxs :: [Double] -> Double -> Text
      showxs qs m =
          (sformat (" " % Formatting.expt 1) m) <> ": " <>
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
