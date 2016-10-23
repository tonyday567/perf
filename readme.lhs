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

> 
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

tickn
===


Let's measure something.  The simplest something I could think of was ().

> 
>   _ <- replicateM 10 tick_
>   (xs, _) <- tickn 10000 ()
>   (xs', _) <- tickn 10000 (pure () :: IO ())
>   let qs = L.fold (quantiles' 11) (fromIntegral <$> xs)
>   let qs' = L.fold (quantiles' 11) (fromIntegral <$> xs')
>   writeFile "other/tickn.md" $ code
>       [ mconcat (sformat (" " % prec 3) <$> qs)
>       , mconcat (sformat (" " % prec 3) <$> qs')
>       ]

quantiles for () and IO ()

```include
other/tickn.md
```

>   let expSum n m = do
>         let s x = foldl' (+) 0 [1..x]
>         (cs,_) <- tickn n (s m)
>         code $ "summing " ++ sci' m ++ " Ints " ++ sci' n ++ " times " ++
>           sci' (average $ cycles <$> cs) ++ " cycles"
>
>   mapM_ (expSum 10000 . (\x -> 10^x)) [0..3]




helpers
---

>
> code cs = mconcat $ (<> "\n") <$> ("    " <>) <$> cs
>


> {-

>   h2 "summing"

Now we try a sum experiment using foldl'.  We're looking for results that are linear in the list size.  Confusingly, this is often referred to as constant time.

I'm cautious of throwing away the results of the computation.  My bangs might be in the right place, but would be great to get some strong guarantees.  I'm also leaving the conceptual door open to lazy evaluation

tickn' tricks the memoisation into letting us rerun the harnessed function using function application. Maybe - I tend to let the numbers tell me what is going on and abstract away the why for now.

The experiment tends to show a nice linear scale, with a typical improvement as the list size grows.  The computer gets better at the computation as it repeats - better GC and data placement I presume.

>   let expSum n m = do
>         let s x = foldl' (+) 0 [1..x]
>         (cs,_) <- tickn n (s m)
>         code $ "summing " ++ sci' m ++ " Ints " ++ sci' n ++ " times " ++
>           sci' (average $ cycles <$> cs) ++ " cycles"
>
>   mapM_ (expSum 10000 . (\x -> 10^x)) [0..3]
>


Next, some prime number adding up.


>   h2 "prime experiment: "
>   let expPrime m = do
>         (c, res) <- do
>           let s !x = average $ fromIntegral <$> take x primes
>           tick $ s m
>         code $ "prime!!" ++ sci' m ++ " = " ++ sci' res ++ ": " ++ sci' c ++ " cycles"
>   mapM_ (expPrime . (\x -> 10^x)) [2..5]




>
> -}

A first-cousin of rdtsc, [rdpmc](https://software.intel.com/en-us/forums/software-tuning-performance-optimization-platform-monitoring/topic/595214), offers the possibility to track page faults, cache misses and other such beasties, but lacks an easy entry-point c library.

[formatting](http://hackage.haskell.org/package/formatting)
[foldl](https://hackage.haskell.org/package/foldl)


workflow
---

    stack install && readme && pandoc -f markdown+lhs -t html -i readme.lhs -o index.html --filter pandoc-include
