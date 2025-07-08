[![img](https://img.shields.io/hackage/v/perf.svg)](https://hackage.haskell.org/package/perf) [![img](https://github.com/tonyday567/perf/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/tonyday567/perf/actions/workflows/haskell-ci.yml)


# Features

`perf` is an experimental library with a focus on the low-level empirics of Haskell code performance. If you are looking for a quick and reliable performance benchmark, criterion and tasty-bench are both good choices. If your results are confounding, however, you may need to dig deeper, and this is the problem space of `perf`.

The library:

-   provides a monad transformer, `PerfT`. The criterion API tends towards an atomistic approach - bust code up into snippets, copy-paste into a bench.hs and measure their isolated performance.  In contrast, with `PerfT` performance can be measured within a code snippet&rsquo;s original context. Differing code points can be labelled and measured as part of a single run, encouraging a much faster observation - experimentation - refactor cycle.

-   is polymorphic to what, exactly, is being measured, so that concepts such as counters, debug checks, time and space performance can share treatment.

-   attempts to measure big O for algorithms that can be defined in terms of input size growth.

-   includes live charting of raw performance results via chart-svg and prettychart


# Usage

Probably the best introduction to `perf` is via the perf-explore executable:

    perf-explore

    label1          label2          old result      new result      change
    
    sum             time            9.93e3          7.57e3          improvement

Summing [1..1000] took 9,930 nanoseconds, an improvement versus the on file performance previously measured.

Live charts of raw performance measurement can be obtained via the prettychart library with:

    prettychart-watch --watch --filepath other --port 3566

&#x2026; and pointer your browser at localhost:3566

    perf-explore -n 1000 --nocheck --chart

![img](other/perf.svg)

In this particular measure, there was an improvement, dropping from about 10,000 nanos to 8,600 nanos. Increasing the number of measurements:

    perf-explore -n 20000 --nocheck --chart --chartpath other/perf20000.svg

![img](other/perf20000.svg)

Improvements seem to continue as n increases before stabilising (after a GC perhaps) at 3,500 nanos

    perf-explore -n 20000 --order --nocheck --tasty

    label1          label2          results
    
    sum             time            3.51e3
    
    sum:time 3.5 * O(N1)
    tasty:time: 3510

The order of the computation (`\l -> fap sum [1 .. l]`) is O(N1) and the results are very close to the tasty-bench result.

In comparsion, (\l -> fap (\x -> sum [1 .. x]) l):

    perf-explore --nocheck --sumFuse -n 100000 --chart --chartpath other/perffuse.svg --order

![img](other/perffuse.svg)

    perf-explore --nocheck --sumFuse -n 100000 --order

    label1          label2          results
    
    sumFuse         time            6.78e2
    
    sumFuse:time 0.66 * O(N1)

&#x2026; is much faster. Hooray for list fusion!


# Issues


## fragility

Results, especially for simple computations, are fragile and can show large variance in performance characteristics in identical runs, and across differing compilations. Whether this is due to library flaws or is just the nature of ghc is an open question.


## Statistics

> Obligatory disclaimer: statistics is a tricky matter, there is no one-size-fits-all approach. In the absence of a good theory simplistic approaches are as (un)sound as obscure ones. Those who seek statistical soundness should rather collect raw data and process it themselves using a proper statistical toolbox. Data reported by tasty-bench is only of indicative and comparative significance. ~ [tasty-bench](https://hackage.haskell.org/package/tasty-bench-0.4/docs/Test-Tasty-Bench.html#t:Benchmarkable)

> variance introduced by outliers: 88% (severely inflated) ~ [criterion](https://hackage.haskell.org/package/criterion)

The library default is to report the 10th percentile as a summary statistic, and this is a matter of taste, determined mostly by the purpose of the measurement.


## ffap and fap

    :t ffap

    ffap
      :: (Control.DeepSeq.NFData a, Control.DeepSeq.NFData b, MonadIO m,
          Semigroup t) =>
         Text.Text -> (a -> b) -> a -> PerfT m t b

ffap and fap are broadly similar to criterion&rsquo;s nf and whnf respectively, but passes throught the results of the computation into the monad transformer, enabling in-context measurement.

A fine-grained and detailed examination of the effect of measurement on laziness and on core details would be beneficial to the library.


## tasty

The library was originally developed before tasty-bench, which does a great job of integrating into the tasty api, and a future refactor may integrate with this, rather than supply idiosyncratic methods.


## order

BigOrder calculations tend to be fragile and sometimes differ from theory.


# Development

This org file has been used to develop and document library innovation and testing, and may be of use to users in understanding the library. Note that running `perf` via ghci is very slow compared with an external process which accesses the compiled version of the library.

    :r
    :set -Wno-type-defaults
    :set -Wno-unused-do-bind
    :set -Wno-name-shadowing
    :set -XOverloadedStrings
    :set -XOverloadedLabels
    import Perf
    import Perf.Report
    import Data.FormatN
    import qualified Data.Text as Text
    import qualified Data.Text.IO as Text
    import qualified Data.Map.Strict as Map
    import Control.Monad
    import Data.Bifunctor
    import System.Clock
    import Data.List qualified as List
    import Control.Category ((>>>))
    import Optics.Core
    import Data.Foldable
    import NumHask.Space
    putStrLn "ok"
    import Chart hiding (tick)
    import Prettychart
    import Chart.Examples
    import Perf.Chart
    (disp,q) <- startChartServer Nothing
    disp lineExample
    import Prettyprinter
    import Control.Monad.State.Lazy
    import Text.PrettyPrint.Boxes

    Ok, 11 modules loaded.
    ok
    Setting phasegrhsc it>o  stun... (poTrrtu e9
    160) (cgthrcli->c  to quitg)h

    l = 1000
    n = 1000
    
    :{
    p = do
      ffap "sum" sum [1 .. l]
      ffap "sumfuse" (\x -> sum [1 .. x]) l
    :}
    :t p
    run = runPerfT (times n) p
    :t run
    (res, m) <- run
    :t m
    median . fmap fromIntegral <$> m

    ghci| ghci| ghci| ghci| ghci> p :: (MonadIO m, Semigroup t, Control.DeepSeq.NFData b, Num b,
          Enum b) =>
         PerfT m t b
    run
      :: (Control.DeepSeq.NFData a, Num a, Enum a) =>
         IO (a, Map.Map Text.Text [Nanos])
    m :: Map.Map Text.Text [Nanos]
    fromList [("sum",21978.1),("sumfuse",26710.18)]


# Details


## System.Clock

The default clock is MonoticRaw for linux & macOS, and ThreadCPUTime for Windows.


### resolution

    getRes Monotonic
    getRes Realtime
    getRes ProcessCPUTime
    getRes ThreadCPUTime
    getRes MonotonicRaw

    TimeSpec {sec = 0, nsec = 1000}
    TimeSpec {sec = 0, nsec = 1000}
    TimeSpec {sec = 0, nsec = 1000}
    TimeSpec {sec = 0, nsec = 42}
    TimeSpec {sec = 0, nsec = 42}


## ticks

The various versions of tick and a variety of algorithms are artifacts of ongoing exploration.

    perf-explore -n 20000 --best --ticks

    algo          stepTime   tick tickForce tickForceArgs tickLazy tickWHNF  times timesn
    sumAux          3.11e3 3.11e3    3.11e3        3.11e3   5.13e0   3.11e3 3.11e3 3.10e3
    sumCata         3.11e3 3.11e3    3.11e3        3.11e3   5.11e0   3.11e3 3.11e3 3.14e3
    sumCo           3.11e3 3.11e3    3.11e3        3.11e3   5.06e0   3.11e3 3.11e3 3.08e3
    sumCoCase       3.11e3 3.11e3    3.11e3        3.11e3   5.11e0   3.11e3 3.11e3 3.08e3
    sumCoGo         3.11e3 3.11e3    3.11e3        3.11e3   5.06e0   3.11e3 3.11e3 3.12e3
    sumF            3.48e3 3.49e3    3.46e3        3.46e3   5.06e0   3.48e3 3.48e3 3.48e3
    sumFlip         3.48e3 3.48e3    3.45e3        3.45e3   5.03e0   3.48e3 3.48e3 3.48e3
    sumFlipLazy     3.48e3 3.48e3    3.45e3        3.45e3   4.96e0   3.48e3 3.48e3 3.45e3
    sumFoldr        3.11e3 3.11e3    3.11e3        3.11e3   5.13e0   3.11e3 3.11e3 3.11e3
    sumFuse         6.54e2 6.54e2    6.54e2        6.54e2   5.17e0   6.54e2 6.54e2 6.39e2
    sumFuseFoldl'   6.54e2 6.54e2    6.54e2        6.54e2   5.00e0   6.54e2 6.54e2 6.44e2
    sumFuseFoldr    9.93e2 9.92e2    9.92e2        9.92e2   5.13e0   9.92e2 9.93e2 9.63e2
    sumFusePoly     6.56e2 6.56e2    6.56e2        6.56e2   5.12e0   6.56e2 6.57e2 6.47e2
    sumLambda       3.48e3 3.49e3    3.48e3        3.48e3   5.12e0   3.48e3 3.48e3 3.55e3
    sumMono         3.48e3 3.48e3    3.46e3        3.46e3   5.00e0   3.48e3 3.48e3 3.50e3
    sumPoly         3.62e3 3.49e3    3.54e3        3.56e3   5.04e0   3.71e3 3.62e3 3.70e3
    sumSum          3.48e3 3.49e3    3.48e3        3.48e3   4.98e0   3.48e3 3.48e3 3.49e3
    sumTail         3.48e3 3.49e3    3.45e3        3.45e3   5.00e0   3.48e3 3.48e3 3.51e3
    sumTailLazy     3.48e3 3.48e3    3.45e3        3.45e3   5.16e0   3.48e3 3.48e3 3.49e3


## Time


### What is a tick?

A fundamental operation of Perf.Time is tick, which sandwiches a (strict) function application between two readings of a clock, and returns time in nanoseconds, and the computation result. In this way, the \`Perf\` monad can be inserted into the midst of a computation in an attempt to measure performance in-situ as opposed to sitting off in a separate and decontextualized process.

    :t tick

    tick :: (a -> b) -> a -> IO (Nanos, b)

`tick` returns in the IO monad, because reading a cycle counter is an IO effect. A trivial but fundamental point is that performance measurement effects the computation being measured.


### tick\_

tick\_ measures the nanoseconds between two immediate clock reads.

    :t tick_

    tick_ :: IO Nanos

    replicateM 10 tick_

    [1833,500,416,416,416,375,375,416,416,416]


### multiple ticks

    fmap (fmap (fst)) . replicateM 10 $ tick (const ()) ()

    [7000,2333,2000,2208,1958,1959,1959,2000,2000,1959]

Here, `const () ()` was evaluated and took 7 micro-seconds for the first effect, reducing down to 2 msecs after 10 effects.


### tickIO

`tickIO` measures the evaluation of an IO value.

    :t tickIO

    tickIO :: IO a -> IO (Cycles, a)

    fmap (fmap fst) . replicateM 10 $ tickIO (pure ())

    [5541,1625,1458,1833,1375,1416,1375,1375,1375,1375]


### sum example

    fmap (expt (Just 2) . fromIntegral) . fst <$> ticks 10 sum ([1..10000] :: [Double])

    ["5.0e5","2.4e5","2.4e5","2.4e5","2.4e5","2.4e5","2.4e5","2.4e5","2.5e5","2.4e5"]

    ts <- ticks 10000 sum ([1..1000] :: [Double])
    print $ average (fmap fromIntegral $ fst ts)

    10747.1975


## PerfT

`PerfT` allows for multiple measurement points and is polymorphic in what is being measured. It returns a Map of results held in State.

Compare a lower-level usage of ticks, measuring the average of summing to one thousand over one thousand trials:

    first (average . fmap fromIntegral) <$> ticks 1000 sum [1..1000]

    (25947.635,500500)

&#x2026; with PerfT usage

    second (fmap (average . fmap fromIntegral)) <$> runPerfT (times 1000) (sum |$| [1..1000])

    (500500,fromList [("",26217.098)])

An IO example

    exampleIO' :: IO ()
    exampleIO' = do
      txt <- Text.readFile "src/Perf.hs"
      let n = Text.length txt
      Text.putStrLn $ "length of file is: " <> Text.pack (show n)

    exampleIO = execPerfT time (do
      txt <- fam "file_read" (Text.readFile "src/Perf.hs")
      n <- fap "length" Text.length txt
      fam "print_result" (Text.putStrLn $ "length of file is: " <> Text.pack (show n)))

    perf-explore --exampleIO

    length of file is: 1794
    length of file is: 1794
    
    label1          label2          label3          old result      new result      change
    
    normal          file-read       time            2.31e5          1.28e5          improvement
    normal          length          time            2.71e3          2.00e3          improvement
    normal          print-result    time            3.75e4          1.32e4          improvement
    outer           file-read       time            6.05e4          3.64e4          improvement
    outer           length          time            9.59e2          6.25e2          improvement
    outer           outer-total     time            7.39e4          4.02e4          improvement
    outer           print-result    time            9.79e3          1.71e3          improvement


## Perf.BigO

Perf.BigO represents functionality to determine the complexity order for a computation.

We could do a regression and minimise the error term, but we know that the largest run contains the most information; we would need to weight the simulations according to some heuristic.

Instead, we:

-   estimate the order factor for each possible Order, from N3 to N0, setting the highest n run constant factor to zero,
-   pick the order based on lowest absolute error result summed across all the runs,

    import qualified Prelude as P
    import Data.List (nub)
    estOrder (\x -> sum $ nub [1..x]) 100 [10,100,1000,1000]

    BigOrder {bigOrder = N2, bigFactor = 3.187417}

    import qualified Prelude as P
    import Data.List (nub)
    estOrder (\x -> sum $ [1..x]) 10 [1,10,100,1000]

    BigOrder {bigOrder = N12, bigFactor = 695.0370069284081, bigConstant = 0.0}


## References

<https://wiki.haskell.org/Performance/GHC>

[The Haskell performance checklist](https://github.com/haskell-perf/checklist)

[ndmitchell/spaceleak: Notes on space leaks](https://github.com/ndmitchell/spaceleak)


### Core

[5.13. Debugging the compiler](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/debugging.html#options-debugging)

    ghc app/speed.hs -ddump-simpl -ddump-to-file -fforce-recomp -dlint -O

[haskell wiki: Looking at the Core](https://wiki.haskell.org/Performance/GHC#Looking_at_the_Core)

[godbolt](https://godbolt.org/)

[ghc issue 15185: Enum instance for IntX / WordX are inefficient](https://gitlab.haskell.org/ghc/ghc/-/issues/15185)

[fixpt - All About Strictness Analysis (part 1)](https://fixpt.de/blog/2017-12-04-strictness-analysis-part-1.html)


### Profiling

1.  setup

    [8. Profiling](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/profiling.html#prof-heap)
    
    A typical configuration step for profiling:
    
        cabal configure --enable-library-profiling --enable-executable-profiling -fprof-auto -fprof -write-ghc-environment-files=always
    
    A cabal.project.local with profiling enabled:
    
    > write-ghc-environment-files: always
    > ignore-project: False
    > flags: +prof +prof-auto
    > library-profiling: True
    > executable-profiling: True
    
    Examples from markup-parse R&D:
    
    Executable compilation:
    
        ghc -prof -fprof-auto -rtsopts app/speed0.hs -threaded -fforce-recomp
    
    Executable run:
    
        app/speed0 +RTS -s -p -hc -l -RTS

2.  Space usage output (-s)

        885,263,472 bytes allocated in the heap
               8,507,448 bytes copied during GC
                 163,200 bytes maximum residency (4 sample(s))
                  27,752 bytes maximum slop
                       6 MiB total memory in use (0 MiB lost due to fragmentation)
        
                                             Tot time (elapsed)  Avg pause  Max pause
          Gen  0       207 colls,     0 par    0.009s   0.010s     0.0001s    0.0002s
          Gen  1         4 colls,     0 par    0.001s   0.001s     0.0004s    0.0005s
        
          TASKS: 4 (1 bound, 3 peak workers (3 total), using -N1)
        
          SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)
        
          INIT    time    0.006s  (  0.006s elapsed)
          MUT     time    0.367s  (  0.360s elapsed)
          GC      time    0.010s  (  0.011s elapsed)
          RP      time    0.000s  (  0.000s elapsed)
          PROF    time    0.000s  (  0.000s elapsed)
          EXIT    time    0.001s  (  0.001s elapsed)
          Total   time    0.384s  (  0.380s elapsed)

3.  Cost center profile (-p)

    Dumped to speed0.prof
    
        COST CENTRE MODULE                SRC                                            %time %alloc
        
        token       MarkupParse           src/MarkupParse.hs:(259,1)-(260,20)             50.2   50.4
        wrappedQ'   MarkupParse.FlatParse src/MarkupParse/FlatParse.hs:(215,1)-(217,78)   20.8   23.1
        ws_         MarkupParse.FlatParse src/MarkupParse/FlatParse.hs:(135,1)-(146,4)    14.3    5.5
        eq          MarkupParse.FlatParse src/MarkupParse/FlatParse.hs:243:1-30           10.6   11.1
        gather      MarkupParse           src/MarkupParse.hs:(420,1)-(428,100)             2.4    3.7
        runParser   FlatParse.Basic       src/FlatParse/Basic.hs:(217,1)-(225,24)          1.0    6.0

4.  heap analysis (-hc -l)

        eventlog2html speed0.eventlog
    
    Produces speed0.eventlog.html which contains heap charts.


### Cache speed

The average cycles per + operation can get down to about 0.7 cycles, and there are about 4 cache registers per cycle, so a sum pipeline uses 2.8 register instructions per +.

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-right" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Cache</th>
<th scope="col" class="org-right">nsecs</th>
<th scope="col" class="org-left">Cycles</th>
</tr>
</thead>
<tbody>
<tr>
<td class="org-left">register</td>
<td class="org-right">0.1</td>
<td class="org-left">4 per cycle</td>
</tr>

<tr>
<td class="org-left">L1 Cache access</td>
<td class="org-right">1</td>
<td class="org-left">3-4 cycles</td>
</tr>

<tr>
<td class="org-left">L2 Cache access</td>
<td class="org-right">4</td>
<td class="org-left">11-12 cycles</td>
</tr>

<tr>
<td class="org-left">L3 unified access</td>
<td class="org-right">14</td>
<td class="org-left">30 - 40</td>
</tr>

<tr>
<td class="org-left">DRAM hit</td>
<td class="org-right">80</td>
<td class="org-left">195 cycles</td>
</tr>

<tr>
<td class="org-left">L1 miss</td>
<td class="org-right">16</td>
<td class="org-left">40 cycles</td>
</tr>

<tr>
<td class="org-left">L2 miss</td>
<td class="org-right">&gt;250</td>
<td class="org-left">&gt;600 cycles</td>
</tr>
</tbody>
</table>

