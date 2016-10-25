```include
other/header.md
```

[perf](https://tonyday567.github.io/perf/index.html) [![Build Status](https://travis-ci.org/tonyday567/perf.png)](https://travis-ci.org/tonyday567/perf)
===

If you want to make stuff very fast in haskell, you need to dig down below the criterion abstraction-level and start counting cycles using the [rdtsc](https://en.wikipedia.org/wiki/Time_Stamp_Counter) register on x86.

> {-# LANGUAGE OverloadedStrings #-}
> import Protolude hiding ((%))
> import qualified Data.Text.IO as Text
> import qualified Data.Text as Text
> import Formatting
> import Online
> import qualified Control.Foldl as L
> import Math.Combinatorics.Exact.Primes
> import Perf.Cycles
> import Chart.Unit hiding ((<>))
> import Chart.Types
> import Control.Lens
> import Data.Default
> import qualified Data.Vector as V
> import qualified Data.Vector.Unboxed as U
> import Data.List
> import Linear
> import qualified Tower as T
> import Data.Primitive.MutVar
> 

main
---

> main :: IO ()
> main = do

`tick_` taps the register twice to get a sense of the cost.

>   onetick <- tick_
>   ticks <- replicateM 10 tick_
>   avtick <- replicateM 1000000 tick_
>   let average cs = L.fold ((/) <$> L.sum <*> L.genericLength) cs
>   Text.writeFile "other/onetick.md" $ code
>     [ "one tick_: " <> (Text.pack $ show onetick) <> " cycles"
>     , "next 10: " <> (Text.pack $ show ticks)
>     , "average over 1m: " <>
>       (Text.pack $ show $ average (fromIntegral <$> avtick)) <> " cycles"
>     ]
>
> 

```include
other/onetick.md
```

It often makes sense to give tick_ a few spins before measuring something, to warm everything up.

A pattern I see on my machine are shifts by multiples of 4, which correspond to roughly the L1 [cache latency](http://stackoverflow.com/questions/1126529/what-is-the-cost-of-an-l1-cache-miss).

It pays to look at the whole distribution, and a compact way of doing that is to calculate quantiles:

>   _ <- warmup 100
>   xs' <- replicateM 10000 tick_
>   let xs = fromIntegral <$> xs' :: [Double]
>   let qs = L.fold (quantiles' 11) xs
>   writeFile "other/quantiles.md" $
>         "\n    [min, 10th, 20th, .. 90th, max]:" <>
>         mconcat (sformat (" " % prec 3) <$> qs)
> 

```include
other/quantiles.md
```

The important cycle count for most work is around the 30th to 50th percentile, where you get a clean measure, hopefully free of cache missing.

The quantile print of tick_ often shows a 12 to 14 point jump around the 70th to 90th percential, and this is probably a L2 miss, and then a few large brain farts at around 2k cycles.

For reference, based on a 2.6G machine one cycle is = 0.38 𝛈s

spin
===

Let's measure something.  The simplest something I could think of was summing.

`spin` takes n measurements of whatever part you want to measure.

>   _ <- warmup 100
>   let f x = foldl' (+) 0 [1..x]
>   let ms = [1, 10, 100, 1000, 10000, 100000]
>   let n = 100
>   res <- sequence $ (spin n tick f) <$> ms
>   let xs = fmap fromIntegral <$> (fst <$> res) :: [[Double]]
>   let qss = L.fold (quantiles' 11) <$> xs
>   let showxs :: [Double] -> Double -> Text
>       showxs qs m =
>           (show m) <> ": " <>
>           mconcat (sformat (" " % prec 3) <$> ((\x -> x/m) <$> qs))
>   Text.writeFile "other/spin.md" $ code $
>       zipWith showxs qss ms


```include
other/spin.md
```

time series
---

>   fileSvg "other/raw1k.svg" (300,300) $
>       rect'
>       def
>       [ rectBorderColor .~ Color 0 0 0 0
>       $ rectColor .~ Color 0.333 0.333 0.333 0.5
>       $ def]
>       [zipWith4 V4 [0..] (cycle [0]) [1..] (xs !! 3)]
>       
>   fileSvg "other/raw100.svg" (300,300) $
>       rect'
>       def
>       [ rectBorderColor .~ Color 0 0 0 0
>       $ rectColor .~ Color 0.333 0.333 0.333 0.5
>       $ def]
>       [zipWith4 V4 [0..] (cycle [0]) [1..] (xs !! 2)]

Individual measurements for m=100

![](other/raw100.svg)

Individual measurements for m=1000

![](other/raw1k.svg)

On my run, a 3e5 cycle process comes along every 3e6 or so and smashes the loop.


Tower
---

`spin n tickf f` takes n measures using the tickf version, which just looks at function application effects.

>   _ <- warmup 100
>   let f x = foldl' (T.+) 0 [1..x]
>   let ms = [1, 10, 100, 1000, 10000, 100000]
>   let n = 100
>   res <- sequence $ (spin n tickf f) <$> ms
>   let xs = fmap fromIntegral <$> (fst <$> res) :: [[Double]]
>   let qss = L.fold (quantiles' 11) <$> xs
>   let showxs :: [Double] -> Double -> Text
>       showxs qs m =
>           (show m) <> ": " <>
>           mconcat (sformat (" " % prec 3) <$> ((\x -> x/m) <$> qs))
>   Text.writeFile "other/ticktower.md" $ code $
>       zipWith showxs qss ms

```include
other/ticktower.md
```

vector
---

Using vector to sum:

>   _ <- warmup 100
>   let f x = V.foldl (+) 0 $ V.replicate x 1
>   let ms = [1, 10, 100, 1000, 10000, 100000]
>   let n = 100
>   res <- sequence $ (spin n tickf f) <$> ms
>   let xs = fmap fromIntegral <$> (fst <$> res) :: [[Double]]
>   let qss = L.fold (quantiles' 11) <$> xs
>   let showxs :: [Double] -> Double -> Text
>       showxs qs m =
>           (show m) <> ": " <>
>           mconcat (sformat (" " % prec 3) <$> ((\x -> x/m) <$> qs))
>   Text.writeFile "other/vector1.md" $ code $
>       zipWith showxs qss (fromIntegral <$> ms)

```include
other/vector1.md
```

unboxed vector
---

Using unboxed vector to sum Ints:

>   _ <- warmup 100
>   let f x = U.foldl' (+) (0::Int) $ U.replicate x 1
>   let ms = [1, 10, 100, 1000, 10000, 100000]
>   let n = 100
>   res <- sequence $ (spin n tickf f) <$> ms
>   let xs = fmap fromIntegral <$> (fst <$> res) :: [[Double]]
>   let qss = L.fold (quantiles' 11) <$> xs
>   let showxs :: [Double] -> Double -> Text
>       showxs qs m =
>           (show m) <> ": " <>
>           mconcat (sformat (" " % prec 3) <$> ((\x -> x/m) <$> qs))
>   Text.writeFile "other/vector2.md" $ code $
>       zipWith showxs qss (fromIntegral <$> ms)

```include
other/vector2.md
```

Peak performance clocks in on my machine around 11 cycles per element. Immutable is the important proviso.

>   fileSvg "other/unboxed1k.svg" (300,300) $
>       rect'
>       def
>       [ rectBorderColor .~ Color 0 0 0 0
>       $ rectColor .~ Color 0.333 0.333 0.333 0.5
>       $ def]
>       [zipWith4 V4 [0..] (cycle [0]) [1..] (xs !! 3)]

Individual measurements for m=1000

![](other/unboxed1k.svg)

We are down to one 3e5 brain fart, but only because our computation is a lot faster now. It's going to be something boring like GC, or an out of order execution effect.  Since it's regular, we can keep an eye on it, and separate the effect whatever it is.

the `!f` and the `!a`
---

Same unboxed Int vector, looking at `tickfa`:

>   _ <- warmup 100
>   let f x = U.foldl' (+) (0::Int) $ U.replicate x 1
>   let ms = [1, 10, 100, 1000, 10000, 100000]
>   let n = 100
>   res <- sequence $ (spin n tickfa f) <$> ms
>   res' <- sequence $ (spin n tick f) <$> ms
>   let xsf = fmap (fromIntegral . fst) <$> (fst <$> res) :: [[Double]]
>   let xsa = fmap (fromIntegral . snd) <$> (fst <$> res) :: [[Double]]
>   let xsb = fmap (fromIntegral) <$> (fst <$> res') :: [[Double]]
>   let qssf = L.fold (quantiles' 11) <$> xsf
>   let qssa = L.fold (quantiles' 11) <$> xsa
>   let qssb = L.fold (quantiles' 11) <$> xsb
>   let showxs :: [Double] -> Double -> Text
>       showxs qs m =
>           (show m) <> ": " <>
>           mconcat (sformat (" " % prec 3) <$> ((\x -> x/m) <$> qs))
>   Text.writeFile "other/f.md" $ code $
>       zipWith showxs qssf (fromIntegral <$> ms)
>   Text.writeFile "other/a.md" $ code $
>       zipWith showxs qssa (fromIntegral <$> ms)
>   Text.writeFile "other/b.md" $ code $
>       zipWith showxs qssb (fromIntegral <$> ms)

function application effect

```include
other/f.md
```

instantiation effect

```include
other/a.md
```

both effects

```include
other/b.md
```

mutation
---

Mutable summer of Ints:

>   _ <- warmup 100
>   let f x = L.foldM mutFold $ V.replicate x (1::Int)
>   let ms = [1, 10, 100, 1000, 10000, 100000]
>   let n = 100
>   res <- sequence $ (spinM n tickfM f) <$> ms
>   let xs = fmap fromIntegral <$> (fst <$> res) :: [[Double]]
>   let xsres = snd <$> res :: [Int]
>   print $ L.fold (L.Fold (+) 0 Protolude.identity) xsres
>   let qss = L.fold (quantiles' 11) <$> xs
>   let showxs :: [Double] -> Double -> Text
>       showxs qs m =
>           (show m) <> ": " <>
>           mconcat (sformat (" " % prec 3) <$> ((\x -> x/m) <$> qs))
>   Text.writeFile "other/mutable.md" $ code $
>       zipWith showxs qss (fromIntegral <$> ms)

```include
other/mutable.md
```




helpers
---

>
> code cs = mconcat $ (<> "\n") <$> ("    " <>) <$> cs
>

> mutFold :: L.FoldM IO Int Int
> mutFold = L.FoldM step begin done
>   where
>     step x a = modifyMutVar' x (+a) >> pure x
>     begin = newMutVar 0
>     done = readMutVar


rdpmc
---

A first-cousin of rdtsc, [rdpmc](https://software.intel.com/en-us/forums/software-tuning-performance-optimization-platform-monitoring/topic/595214), offers the possibility to track page faults, cache misses and other such beasties, but lacks an easy entry-point c library.

workflow
---

    stack install && readme && pandoc -f markdown+lhs -t html -i readme.lhs -o index.html --filter pandoc-include
