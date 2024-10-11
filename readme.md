
# Table of Contents

1.  [Introduction](#orga196864)
2.  [Setup](#orgfd30244)
3.  [System.Clock](#orgb51effe)
    1.  [resolution](#org7dcd69d)
4.  [Time](#org6c19f0f)
    1.  [What is a tick?](#org49fb855)
    2.  [tick\_](#org1de7ebb)
    3.  [multiple ticks](#org27958a7)
    4.  [tickIO](#orga206cb6)
    5.  [sum example](#orgd6c8625)
5.  [PerfT](#org0974e4d)
6.  [perf-explore](#org216f105)
    1.  [noops](#org2d42223)
    2.  [measurement context](#org95a9062)
        1.  [short list](#orgdb37d7c)
        2.  [long list](#org56b0098)
    3.  [sums](#orgff01033)
    4.  [lengths](#org5abd0c1)
    5.  [Space](#org01bde6f)
7.  [Perf.BigO](#org753786d)
8.  [References](#org47311bd)
    1.  [Core](#orgadd7f60)
    2.  [Profiling](#org31e588b)
        1.  [setup](#orgf72792c)
        2.  [Space usage output (-s)](#org1d9ca37)
        3.  [Cost center profile (-p)](#orgeb93acc)
        4.  [heap analysis (-hc -l)](#org76c2a10)
    3.  [Cache speed](#org3a53ed0)

[![img](https://img.shields.io/hackage/v/perf.svg)](https://hackage.haskell.org/package/perf) [![img](https://github.com/tonyday567/perf/workflows/haskell-ci/badge.svg)](https://github.com/tonyday567/perf/actions?query=workflow%3Ahaskell-ci)


<a id="orga196864"></a>

# Introduction

`perf` provides some ideas, code and a library for low-level performance measurement for Haskell hacking. The library:

-   provides a monad transformer, `PerfT`, as a light-weight wrapper for use on existing code. `PerfT` modifications can be included in code bases, as opposed to performance being separated code and process, with any effects able to be erased at compile time with `evalPerfT`.

-   focuses on fast and accurate measurement.

-   is polymorphic to what, exactly, is being measured, so that concepts such as counters, debug checks, time and space performance can share treatment.

-   can measure big O for algorithms that can be defined in terms of input size growth.


<a id="orgfd30244"></a>

# Setup

Note that running perf.org is very slow compared with an external process which accesses the compiled version of the library.

    :r
    :set -Wno-type-defaults
    :set -Wno-unused-do-bind
    :set -Wno-name-shadowing
    :set -XOverloadedStrings
    :set -XOverloadedLabels
    import Perf
    import Data.FormatN
    import qualified Data.Text as Text
    import qualified Data.Text.IO as Text
    import qualified Data.Map.Strict as Map
    import Control.Monad
    import Data.Bifunctor
    import System.Clock
    putStrLn "ok"

    [ 1 of 10] Compiling Perf.Stats       ( src/Perf/Stats.hs, interpreted ) [Source file changed]
    [ 3 of 10] Compiling Perf.Time        ( src/Perf/Time.hs, interpreted ) [Source file changed]
    [ 6 of 10] Compiling Perf.Measure     ( src/Perf/Measure.hs, interpreted ) [Source file changed]
    [ 7 of 10] Compiling Perf.Report      ( src/Perf/Report.hs, interpreted ) [Source file changed]
    [ 8 of 10] Compiling Perf.BigO        ( src/Perf/BigO.hs, interpreted ) [Perf.Stats changed]
    [10 of 10] Compiling Perf             ( src/Perf.hs, interpreted ) [Perf.BigO changed]
    Ok, 10 modules reloaded.
    ok


<a id="orgb51effe"></a>

# System.Clock

The default clock is MonoticRaw for linux & macOS, and ThreadCPUTime for Windows.


<a id="org7dcd69d"></a>

## resolution

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


<a id="org6c19f0f"></a>

# Time


<a id="org49fb855"></a>

## What is a tick?

A fundamental operation of Perf.Time is tick, which sandwiches a (strict) function application between two readings of a clock, and returns time in nanoseconds, and the computation result. In this way, the \`Perf\` monad can be inserted into the midst of a computation in an attempt to measure performance in-situ as opposed to sitting off in a separate and decontextualized process.

    :t tick

    tick :: (a -> b) -> a -> IO (Nanos, b)

`tick` returns in the IO monad, because reading a cycle counter is an IO effect. A trivial but fundamental point is that performance measurement effects the computation being measured.


<a id="org1de7ebb"></a>

## tick\_

tick\_ measures the nanoseconds between two immediate clock reads.

    :t tick_

    tick_ :: IO Nanos

    replicateM 10 tick_

    [1833,500,416,416,416,375,375,416,416,416]


<a id="org27958a7"></a>

## multiple ticks

    fmap (fmap (fst)) . replicateM 10 $ tick (const ()) ()

    [7000,2333,2000,2208,1958,1959,1959,2000,2000,1959]

Here, `const () ()` was evaluated and took 7 micro-seconds for the first effect, reducing down to 2 msecs after 10 effects.


<a id="orga206cb6"></a>

## tickIO

`tickIO` measures the evaluation of an IO value.

    :t tickIO

    tickIO :: IO a -> IO (Cycles, a)

    fmap (fmap fst) . replicateM 10 $ tickIO (pure ())

    [5541,1625,1458,1833,1375,1416,1375,1375,1375,1375]


<a id="orgd6c8625"></a>

## sum example

    fmap (expt (Just 2) . fromIntegral) . fst <$> ticks 10 sum ([1..10000] :: [Double])

    ["5.0e5","2.4e5","2.4e5","2.4e5","2.4e5","2.4e5","2.4e5","2.4e5","2.5e5","2.4e5"]

    ts <- ticks 10000 sum ([1..1000] :: [Double])
    print $ average (fmap fromIntegral $ fst ts)

    10747.1975


<a id="org0974e4d"></a>

# PerfT

`PerfT` allows for multiple measurement points and is polymorphic in what is being measured. It returns a Map of results held in State.

Compare a lower-level usage of ticks, measuring the average of summing to one thousand over one thousand trials:

    first (average . fmap fromIntegral) <$> ticks 1000 sum [1..1000]

    (25947.635,500500)

&#x2026; with PerfT usage

    second (fmap (average . fmap fromIntegral)) <$> runPerfT (times 1000) (sum |$| [1..1000])

    (500500,fromList [("",26217.098)])

Comparing performance of sum versus a list fusion approach:

    fmap (average . fmap fromIntegral) <$> (execPerfT (times 1000) $ do; (fap "sum" sum [1..1000]); (fap "fusion" (\x -> sum [1..x]) 1000))

    fromList [("fusion",32871.248),("sum",26924.128)]

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


<a id="org216f105"></a>

# perf-explore

`perf-explore` contains some exploratory routines used to develop `perf`

    perf-explore --help

    examples of perf usage
    
    Usage: perf-explore [-n|--runs ARG]
                        [--Monotonic | --Realtime | --ProcessCPUTime |
                          --ThreadCPUTime | --MonotonicRaw]
                        [--best | --median | --average]
                        [--time | --space | --spacetime | --allocation | --count]
                        [-g|--golden ARG] [--nocheck] [-r|--record]
                        [--header | --noheader] [--error ARG] [--warning ARG]
                        [--improved ARG]
                        [--sums | --lengths | --nub | --clocks | --examples |
                          --example | --exampleIO | --noops | --ticks]
                        [-l|--length ARG]
                        [--sumFuse | --sum | --lengthF | --constFuse | --mapInc |
                          --noOp]
    
      perf exploration
    
    Available options:
      -n,--runs ARG            number of runs to perform
      --best                   report upper decile
      --median                 report median
      --average                report average
      --time                   measure time performance
      --space                  measure space performance
      --spacetime              measure both space and time performance
      --allocation             measure bytes allocated
      --count                  measure count
      -g,--golden ARG          golden file name
      --nocheck                do not check versus the golden file
      -r,--record              record the result to the golden file
      --header                 include headers
      --noheader               dont include headers
      --error ARG              error level
      --warning ARG            warning level
      --improved ARG           improved level
      --sums                   run on sum algorithms
      --lengths                run on length algorithms
      --nub                    nub test
      --clocks                 clock test
      --examples               run on example algorithms
      --example                run on the example algorithm
      --exampleIO              exampleIO test
      --noops                  noops test
      --ticks                  tick test
      -l,--length ARG          length of list
      --sumFuse                fused sum pipeline
      --sum                    sum
      --lengthF                foldr id length
      --constFuse              fused const pipeline
      --mapInc                 fmap (+1)
      --noOp                   const ()
      -h,--help                Show this help text

    fmap averageI <$> execPerfT (times 10000) (sum |$| [1..1000])

    fromList [("",136055.5594)]

The equivalent to the above code is:

    perf-explore -n 10000 -l 1000 --sum --nocheck

    label1          label2          results
    
    sum             time            6.32e3


<a id="org2d42223"></a>

## noops

This no-op experiment is useful to understand the pure time performance of the machinery around measurement. It can be (re)run with:

    perf-explore --noops

    label1          label2          label3          old result      new result      change
    
    const           1st             time            1.72e4          8.79e3          improvement
    const           2nd             time            2.09e2          1.25e2          improvement
    const           3rd             time            1.66e2          1.25e2          improvement
    const           4th             time            2.08e2          8.30e1          improvement
    const           average         time            2.08e2          1.10e2          improvement
    const           best            time            1.31e2          6.31e1          improvement
    const           median          time            1.60e2          7.76e1          improvement
    pure            1st             time            1.00e3          1.25e2          improvement
    pure            2nd             time            1.67e2          8.30e1          improvement
    pure            3rd             time            1.66e2          8.30e1          improvement
    pure            4th             time            1.25e2          4.20e1          improvement
    pure            average         time            1.85e2          8.29e1          improvement
    pure            best            time            1.31e2          6.37e1          improvement
    pure            median          time            1.63e2          7.79e1          improvement


<a id="org95a9062"></a>

## measurement context

Exploration of how the code surrounding measurement effects performance.

    perf-explore -n 1000 -l 1000 --ticks --nocheck

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-right" />

<col  class="org-right" />

<col  class="org-right" />

<col  class="org-right" />

<col  class="org-right" />

<col  class="org-right" />

<col  class="org-right" />
</colgroup>
<tbody>
<tr>
<td class="org-left">&#xa0;</td>
<td class="org-right">stepTime</td>
<td class="org-right">tick</td>
<td class="org-right">tickForce</td>
<td class="org-right">tickForceArgs</td>
<td class="org-right">tickLazy</td>
<td class="org-right">tickWHNF</td>
<td class="org-right">times</td>
</tr>

<tr>
<td class="org-left">sumAux</td>
<td class="org-right">3.29e3</td>
<td class="org-right">4.83e3</td>
<td class="org-right">3.29e3</td>
<td class="org-right">3.29e3</td>
<td class="org-right">1.85e2</td>
<td class="org-right">3.92e3</td>
<td class="org-right">3.29e3</td>
</tr>

<tr>
<td class="org-left">sumCata</td>
<td class="org-right">5.86e3</td>
<td class="org-right">5.61e3</td>
<td class="org-right">6.00e3</td>
<td class="org-right">6.12e3</td>
<td class="org-right">1.85e2</td>
<td class="org-right">5.78e3</td>
<td class="org-right">5.86e3</td>
</tr>

<tr>
<td class="org-left">sumCo</td>
<td class="org-right">3.73e3</td>
<td class="org-right">4.63e3</td>
<td class="org-right">3.66e3</td>
<td class="org-right">3.66e3</td>
<td class="org-right">1.90e2</td>
<td class="org-right">4.36e3</td>
<td class="org-right">3.72e3</td>
</tr>

<tr>
<td class="org-left">sumCoCase</td>
<td class="org-right">5.08e3</td>
<td class="org-right">5.10e3</td>
<td class="org-right">4.96e3</td>
<td class="org-right">4.95e3</td>
<td class="org-right">1.85e2</td>
<td class="org-right">5.12e3</td>
<td class="org-right">5.11e3</td>
</tr>

<tr>
<td class="org-left">sumCoGo</td>
<td class="org-right">3.47e3</td>
<td class="org-right">4.74e3</td>
<td class="org-right">4.66e3</td>
<td class="org-right">4.64e3</td>
<td class="org-right">1.85e2</td>
<td class="org-right">4.72e3</td>
<td class="org-right">3.29e3</td>
</tr>

<tr>
<td class="org-left">sumF</td>
<td class="org-right">5.92e3</td>
<td class="org-right">4.85e3</td>
<td class="org-right">4.84e3</td>
<td class="org-right">6.41e3</td>
<td class="org-right">1.85e2</td>
<td class="org-right">4.85e3</td>
<td class="org-right">5.91e3</td>
</tr>

<tr>
<td class="org-left">sumFlip</td>
<td class="org-right">4.54e3</td>
<td class="org-right">4.45e3</td>
<td class="org-right">4.44e3</td>
<td class="org-right">4.44e3</td>
<td class="org-right">1.85e2</td>
<td class="org-right">4.44e3</td>
<td class="org-right">4.26e3</td>
</tr>

<tr>
<td class="org-left">sumFlipLazy</td>
<td class="org-right">4.52e3</td>
<td class="org-right">4.51e3</td>
<td class="org-right">4.47e3</td>
<td class="org-right">4.47e3</td>
<td class="org-right">1.85e2</td>
<td class="org-right">4.49e3</td>
<td class="org-right">4.50e3</td>
</tr>

<tr>
<td class="org-left">sumFoldr</td>
<td class="org-right">5.55e3</td>
<td class="org-right">4.78e3</td>
<td class="org-right">4.71e3</td>
<td class="org-right">4.72e3</td>
<td class="org-right">1.85e2</td>
<td class="org-right">4.77e3</td>
<td class="org-right">5.56e3</td>
</tr>

<tr>
<td class="org-left">sumFuse</td>
<td class="org-right">8.28e2</td>
<td class="org-right">8.33e2</td>
<td class="org-right">8.29e2</td>
<td class="org-right">8.29e2</td>
<td class="org-right">1.86e2</td>
<td class="org-right">8.28e2</td>
<td class="org-right">8.29e2</td>
</tr>

<tr>
<td class="org-left">sumFuseFoldl&rsquo;</td>
<td class="org-right">2.03e3</td>
<td class="org-right">8.29e2</td>
<td class="org-right">8.32e2</td>
<td class="org-right">8.29e2</td>
<td class="org-right">1.84e2</td>
<td class="org-right">8.29e2</td>
<td class="org-right">8.29e2</td>
</tr>

<tr>
<td class="org-left">sumFuseFoldr</td>
<td class="org-right">1.17e3</td>
<td class="org-right">1.17e3</td>
<td class="org-right">1.18e3</td>
<td class="org-right">1.17e3</td>
<td class="org-right">1.84e2</td>
<td class="org-right">1.19e3</td>
<td class="org-right">1.17e3</td>
</tr>

<tr>
<td class="org-left">sumFusePoly</td>
<td class="org-right">8.40e2</td>
<td class="org-right">8.37e2</td>
<td class="org-right">8.35e2</td>
<td class="org-right">8.36e2</td>
<td class="org-right">1.84e2</td>
<td class="org-right">8.40e2</td>
<td class="org-right">8.37e2</td>
</tr>

<tr>
<td class="org-left">sumLambda</td>
<td class="org-right">3.67e3</td>
<td class="org-right">5.03e3</td>
<td class="org-right">3.67e3</td>
<td class="org-right">3.67e3</td>
<td class="org-right">1.85e2</td>
<td class="org-right">3.78e3</td>
<td class="org-right">3.67e3</td>
</tr>

<tr>
<td class="org-left">sumMono</td>
<td class="org-right">3.66e3</td>
<td class="org-right">5.13e3</td>
<td class="org-right">5.12e3</td>
<td class="org-right">7.20e3</td>
<td class="org-right">1.84e2</td>
<td class="org-right">5.13e3</td>
<td class="org-right">3.66e3</td>
</tr>

<tr>
<td class="org-left">sumPoly</td>
<td class="org-right">4.83e3</td>
<td class="org-right">4.85e3</td>
<td class="org-right">4.83e3</td>
<td class="org-right">4.84e3</td>
<td class="org-right">1.86e2</td>
<td class="org-right">4.84e3</td>
<td class="org-right">4.84e3</td>
</tr>

<tr>
<td class="org-left">sumSum</td>
<td class="org-right">4.55e3</td>
<td class="org-right">4.83e3</td>
<td class="org-right">4.53e3</td>
<td class="org-right">4.53e3</td>
<td class="org-right">1.85e2</td>
<td class="org-right">6.02e3</td>
<td class="org-right">4.55e3</td>
</tr>

<tr>
<td class="org-left">sumTail</td>
<td class="org-right">4.54e3</td>
<td class="org-right">7.07e3</td>
<td class="org-right">5.81e3</td>
<td class="org-right">4.96e3</td>
<td class="org-right">3.27e2</td>
<td class="org-right">6.49e3</td>
<td class="org-right">4.43e3</td>
</tr>

<tr>
<td class="org-left">sumTailLazy</td>
<td class="org-right">6.24e3</td>
<td class="org-right">4.41e3</td>
<td class="org-right">6.47e3</td>
<td class="org-right">6.23e3</td>
<td class="org-right">2.03e2</td>
<td class="org-right">5.49e3</td>
<td class="org-right">6.24e3</td>
</tr>
</tbody>
</table>


<a id="orgdb37d7c"></a>

### short list

    perf-explore -n 10000 -l 10 --median --ticks

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-right" />

<col  class="org-right" />

<col  class="org-right" />

<col  class="org-right" />

<col  class="org-right" />

<col  class="org-right" />

<col  class="org-right" />
</colgroup>
<tbody>
<tr>
<td class="org-left">&#xa0;</td>
<td class="org-right">stepTime</td>
<td class="org-right">tick</td>
<td class="org-right">tickForce</td>
<td class="org-right">tickForceArgs</td>
<td class="org-right">tickLazy</td>
<td class="org-right">tickWHNF</td>
<td class="org-right">times</td>
</tr>

<tr>
<td class="org-left">sumAux</td>
<td class="org-right">2.17e2</td>
<td class="org-right">2.21e2</td>
<td class="org-right">2.18e2</td>
<td class="org-right">2.19e2</td>
<td class="org-right">1.85e2</td>
<td class="org-right">2.17e2</td>
<td class="org-right">2.18e2</td>
</tr>

<tr>
<td class="org-left">sumCata</td>
<td class="org-right">2.16e2</td>
<td class="org-right">2.19e2</td>
<td class="org-right">2.20e2</td>
<td class="org-right">2.21e2</td>
<td class="org-right">1.85e2</td>
<td class="org-right">2.17e2</td>
<td class="org-right">2.18e2</td>
</tr>

<tr>
<td class="org-left">sumCo</td>
<td class="org-right">2.22e2</td>
<td class="org-right">2.34e2</td>
<td class="org-right">2.17e2</td>
<td class="org-right">2.18e2</td>
<td class="org-right">1.85e2</td>
<td class="org-right">2.17e2</td>
<td class="org-right">2.21e2</td>
</tr>

<tr>
<td class="org-left">sumCoCase</td>
<td class="org-right">2.15e2</td>
<td class="org-right">2.32e2</td>
<td class="org-right">2.18e2</td>
<td class="org-right">2.36e2</td>
<td class="org-right">1.91e2</td>
<td class="org-right">2.18e2</td>
<td class="org-right">2.18e2</td>
</tr>

<tr>
<td class="org-left">sumCoGo</td>
<td class="org-right">2.16e2</td>
<td class="org-right">2.23e2</td>
<td class="org-right">2.18e2</td>
<td class="org-right">2.31e2</td>
<td class="org-right">1.87e2</td>
<td class="org-right">2.16e2</td>
<td class="org-right">2.18e2</td>
</tr>

<tr>
<td class="org-left">sumF</td>
<td class="org-right">2.19e2</td>
<td class="org-right">2.30e2</td>
<td class="org-right">2.19e2</td>
<td class="org-right">2.20e2</td>
<td class="org-right">1.86e2</td>
<td class="org-right">2.19e2</td>
<td class="org-right">2.20e2</td>
</tr>

<tr>
<td class="org-left">sumFlip</td>
<td class="org-right">2.16e2</td>
<td class="org-right">2.34e2</td>
<td class="org-right">2.17e2</td>
<td class="org-right">2.16e2</td>
<td class="org-right">1.85e2</td>
<td class="org-right">2.17e2</td>
<td class="org-right">2.17e2</td>
</tr>

<tr>
<td class="org-left">sumFlipLazy</td>
<td class="org-right">2.16e2</td>
<td class="org-right">2.23e2</td>
<td class="org-right">2.16e2</td>
<td class="org-right">2.17e2</td>
<td class="org-right">1.85e2</td>
<td class="org-right">2.17e2</td>
<td class="org-right">2.18e2</td>
</tr>

<tr>
<td class="org-left">sumFoldr</td>
<td class="org-right">2.14e2</td>
<td class="org-right">2.31e2</td>
<td class="org-right">2.17e2</td>
<td class="org-right">2.17e2</td>
<td class="org-right">1.85e2</td>
<td class="org-right">2.17e2</td>
<td class="org-right">2.18e2</td>
</tr>

<tr>
<td class="org-left">sumFuse</td>
<td class="org-right">2.02e2</td>
<td class="org-right">2.03e2</td>
<td class="org-right">2.03e2</td>
<td class="org-right">2.03e2</td>
<td class="org-right">1.85e2</td>
<td class="org-right">2.03e2</td>
<td class="org-right">2.03e2</td>
</tr>

<tr>
<td class="org-left">sumFuseFoldl&rsquo;</td>
<td class="org-right">2.02e2</td>
<td class="org-right">2.03e2</td>
<td class="org-right">2.03e2</td>
<td class="org-right">2.03e2</td>
<td class="org-right">1.85e2</td>
<td class="org-right">2.03e2</td>
<td class="org-right">2.03e2</td>
</tr>

<tr>
<td class="org-left">sumFuseFoldr</td>
<td class="org-right">2.04e2</td>
<td class="org-right">2.04e2</td>
<td class="org-right">2.07e2</td>
<td class="org-right">2.04e2</td>
<td class="org-right">1.94e2</td>
<td class="org-right">2.05e2</td>
<td class="org-right">2.04e2</td>
</tr>

<tr>
<td class="org-left">sumFusePoly</td>
<td class="org-right">2.05e2</td>
<td class="org-right">2.05e2</td>
<td class="org-right">2.05e2</td>
<td class="org-right">2.05e2</td>
<td class="org-right">1.85e2</td>
<td class="org-right">2.05e2</td>
<td class="org-right">2.05e2</td>
</tr>

<tr>
<td class="org-left">sumLambda</td>
<td class="org-right">2.18e2</td>
<td class="org-right">2.39e2</td>
<td class="org-right">2.18e2</td>
<td class="org-right">2.19e2</td>
<td class="org-right">1.84e2</td>
<td class="org-right">2.20e2</td>
<td class="org-right">2.19e2</td>
</tr>

<tr>
<td class="org-left">sumMono</td>
<td class="org-right">2.08e2</td>
<td class="org-right">2.31e2</td>
<td class="org-right">2.08e2</td>
<td class="org-right">2.11e2</td>
<td class="org-right">1.92e2</td>
<td class="org-right">2.09e2</td>
<td class="org-right">2.09e2</td>
</tr>

<tr>
<td class="org-left">sumPoly</td>
<td class="org-right">2.18e2</td>
<td class="org-right">2.32e2</td>
<td class="org-right">2.19e2</td>
<td class="org-right">2.19e2</td>
<td class="org-right">1.85e2</td>
<td class="org-right">2.20e2</td>
<td class="org-right">2.20e2</td>
</tr>

<tr>
<td class="org-left">sumSum</td>
<td class="org-right">2.18e2</td>
<td class="org-right">2.33e2</td>
<td class="org-right">2.19e2</td>
<td class="org-right">2.19e2</td>
<td class="org-right">1.85e2</td>
<td class="org-right">2.20e2</td>
<td class="org-right">2.19e2</td>
</tr>

<tr>
<td class="org-left">sumTail</td>
<td class="org-right">2.52e2</td>
<td class="org-right">4.19e2</td>
<td class="org-right">2.95e2</td>
<td class="org-right">2.60e2</td>
<td class="org-right">2.69e2</td>
<td class="org-right">3.64e2</td>
<td class="org-right">2.42e2</td>
</tr>

<tr>
<td class="org-left">sumTailLazy</td>
<td class="org-right">2.09e2</td>
<td class="org-right">2.42e2</td>
<td class="org-right">2.13e2</td>
<td class="org-right">2.10e2</td>
<td class="org-right">1.90e2</td>
<td class="org-right">2.28e2</td>
<td class="org-right">2.11e2</td>
</tr>
</tbody>
</table>


<a id="org56b0098"></a>

### long list

    perf-explore -n 100 -l 100000 --best --ticks

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-right" />

<col  class="org-right" />

<col  class="org-right" />

<col  class="org-right" />

<col  class="org-right" />

<col  class="org-right" />

<col  class="org-right" />
</colgroup>
<tbody>
<tr>
<td class="org-left">&#xa0;</td>
<td class="org-right">stepTime</td>
<td class="org-right">tick</td>
<td class="org-right">tickForce</td>
<td class="org-right">tickForceArgs</td>
<td class="org-right">tickLazy</td>
<td class="org-right">tickWHNF</td>
<td class="org-right">times</td>
</tr>

<tr>
<td class="org-left">sumAux</td>
<td class="org-right">7.38e5</td>
<td class="org-right">7.38e5</td>
<td class="org-right">7.36e5</td>
<td class="org-right">7.36e5</td>
<td class="org-right">1.66e2</td>
<td class="org-right">7.38e5</td>
<td class="org-right">7.38e5</td>
</tr>

<tr>
<td class="org-left">sumCata</td>
<td class="org-right">7.40e5</td>
<td class="org-right">7.40e5</td>
<td class="org-right">7.38e5</td>
<td class="org-right">7.39e5</td>
<td class="org-right">1.66e2</td>
<td class="org-right">7.40e5</td>
<td class="org-right">7.40e5</td>
</tr>

<tr>
<td class="org-left">sumCo</td>
<td class="org-right">7.40e5</td>
<td class="org-right">7.41e5</td>
<td class="org-right">7.38e5</td>
<td class="org-right">7.38e5</td>
<td class="org-right">1.66e2</td>
<td class="org-right">7.41e5</td>
<td class="org-right">7.39e5</td>
</tr>

<tr>
<td class="org-left">sumCoCase</td>
<td class="org-right">7.39e5</td>
<td class="org-right">7.39e5</td>
<td class="org-right">7.36e5</td>
<td class="org-right">7.36e5</td>
<td class="org-right">1.66e2</td>
<td class="org-right">7.40e5</td>
<td class="org-right">7.38e5</td>
</tr>

<tr>
<td class="org-left">sumCoGo</td>
<td class="org-right">7.39e5</td>
<td class="org-right">7.39e5</td>
<td class="org-right">7.36e5</td>
<td class="org-right">7.36e5</td>
<td class="org-right">1.66e2</td>
<td class="org-right">7.39e5</td>
<td class="org-right">7.39e5</td>
</tr>

<tr>
<td class="org-left">sumF</td>
<td class="org-right">3.52e5</td>
<td class="org-right">3.52e5</td>
<td class="org-right">3.52e5</td>
<td class="org-right">3.52e5</td>
<td class="org-right">1.66e2</td>
<td class="org-right">3.52e5</td>
<td class="org-right">3.52e5</td>
</tr>

<tr>
<td class="org-left">sumFlip</td>
<td class="org-right">3.75e5</td>
<td class="org-right">3.75e5</td>
<td class="org-right">3.75e5</td>
<td class="org-right">3.75e5</td>
<td class="org-right">1.66e2</td>
<td class="org-right">3.75e5</td>
<td class="org-right">3.75e5</td>
</tr>

<tr>
<td class="org-left">sumFlipLazy</td>
<td class="org-right">3.65e5</td>
<td class="org-right">3.65e5</td>
<td class="org-right">3.65e5</td>
<td class="org-right">3.65e5</td>
<td class="org-right">1.66e2</td>
<td class="org-right">3.65e5</td>
<td class="org-right">3.65e5</td>
</tr>

<tr>
<td class="org-left">sumFoldr</td>
<td class="org-right">7.51e5</td>
<td class="org-right">7.52e5</td>
<td class="org-right">7.47e5</td>
<td class="org-right">7.48e5</td>
<td class="org-right">1.66e2</td>
<td class="org-right">7.51e5</td>
<td class="org-right">7.51e5</td>
</tr>

<tr>
<td class="org-left">sumFuse</td>
<td class="org-right">6.26e4</td>
<td class="org-right">6.26e4</td>
<td class="org-right">6.26e4</td>
<td class="org-right">6.26e4</td>
<td class="org-right">1.66e2</td>
<td class="org-right">6.26e4</td>
<td class="org-right">6.26e4</td>
</tr>

<tr>
<td class="org-left">sumFuseFoldl&rsquo;</td>
<td class="org-right">6.26e4</td>
<td class="org-right">6.26e4</td>
<td class="org-right">6.26e4</td>
<td class="org-right">6.26e4</td>
<td class="org-right">1.66e2</td>
<td class="org-right">6.26e4</td>
<td class="org-right">6.26e4</td>
</tr>

<tr>
<td class="org-left">sumFuseFoldr</td>
<td class="org-right">4.97e5</td>
<td class="org-right">4.95e5</td>
<td class="org-right">4.96e5</td>
<td class="org-right">4.97e5</td>
<td class="org-right">1.66e2</td>
<td class="org-right">4.96e5</td>
<td class="org-right">4.97e5</td>
</tr>

<tr>
<td class="org-left">sumFusePoly</td>
<td class="org-right">6.26e4</td>
<td class="org-right">6.26e4</td>
<td class="org-right">6.26e4</td>
<td class="org-right">6.26e4</td>
<td class="org-right">1.66e2</td>
<td class="org-right">6.26e4</td>
<td class="org-right">6.26e4</td>
</tr>

<tr>
<td class="org-left">sumLambda</td>
<td class="org-right">3.73e5</td>
<td class="org-right">3.71e5</td>
<td class="org-right">3.71e5</td>
<td class="org-right">3.71e5</td>
<td class="org-right">1.66e2</td>
<td class="org-right">3.71e5</td>
<td class="org-right">3.73e5</td>
</tr>

<tr>
<td class="org-left">sumMono</td>
<td class="org-right">3.95e5</td>
<td class="org-right">3.95e5</td>
<td class="org-right">3.95e5</td>
<td class="org-right">3.95e5</td>
<td class="org-right">1.66e2</td>
<td class="org-right">3.95e5</td>
<td class="org-right">3.95e5</td>
</tr>

<tr>
<td class="org-left">sumPoly</td>
<td class="org-right">3.85e5</td>
<td class="org-right">3.85e5</td>
<td class="org-right">3.84e5</td>
<td class="org-right">3.84e5</td>
<td class="org-right">1.66e2</td>
<td class="org-right">3.85e5</td>
<td class="org-right">3.85e5</td>
</tr>

<tr>
<td class="org-left">sumSum</td>
<td class="org-right">4.06e5</td>
<td class="org-right">4.06e5</td>
<td class="org-right">4.06e5</td>
<td class="org-right">4.06e5</td>
<td class="org-right">1.66e2</td>
<td class="org-right">4.06e5</td>
<td class="org-right">4.06e5</td>
</tr>

<tr>
<td class="org-left">sumTail</td>
<td class="org-right">3.06e5</td>
<td class="org-right">3.53e5</td>
<td class="org-right">3.06e5</td>
<td class="org-right">3.06e5</td>
<td class="org-right">1.66e2</td>
<td class="org-right">3.08e5</td>
<td class="org-right">3.06e5</td>
</tr>

<tr>
<td class="org-left">sumTailLazy</td>
<td class="org-right">3.01e5</td>
<td class="org-right">3.01e5</td>
<td class="org-right">3.01e5</td>
<td class="org-right">3.01e5</td>
<td class="org-right">1.66e2</td>
<td class="org-right">3.01e5</td>
<td class="org-right">3.01e5</td>
</tr>
</tbody>
</table>


<a id="orgff01033"></a>

## sums

    perf-explore -n 1000 -l 1000 --sums

    label1          label2          old result      new result      change
    
    sumAux          time            5.53e3          5.21e3          improvement
    sumCata         time            5.18e3          4.73e3          improvement
    sumCo           time            6.50e3          6.40e3
    sumCoCase       time            5.16e3          6.03e3          slightly-degraded
    sumCoGo         time            6.11e3          5.88e3
    sumF            time            5.44e3          4.16e3          improvement
    sumFlip         time            7.23e3          7.07e3
    sumFlipLazy     time            6.68e3          6.44e3
    sumFoldr        time            5.23e3          5.00e3
    sumFuse         time            6.85e2          6.81e2
    sumFuseFoldl'   time            6.94e2          6.78e2
    sumFuseFoldr    time            1.04e3          1.02e3
    sumFusePoly     time            6.96e2          6.84e2
    sumLambda       time            4.79e3          4.77e3
    sumMono         time            4.82e3          4.84e3
    sumPoly         time            4.77e3          4.81e3
    sumSum          time            4.95e3          5.05e3
    sumTail         time            7.32e3          7.10e3
    sumTailLazy     time            6.75e3          6.52e3


<a id="org5abd0c1"></a>

## lengths

    perf-explore -n 1000 -l 1000 --lengths

    label1          label2          old result      new result      change          
    
    lengthAux       time            4.44e3          3.68e3          improvement     
    lengthCo        time            4.91e3          4.45e3          improvement     
    lengthCoCase    time            4.90e3          4.44e3          improvement
    lengthF         time            3.38e3          3.21e3
    lengthFMono     time            4.16e3          4.00e3
    lengthFlip      time            5.49e3          4.90e3          improvement
    lengthFlipLazy  time            5.32e3          4.77e3          improvement
    lengthFoldr     time            4.23e3          3.90e3          improvement
    lengthFoldrConsttime            3.98e3          3.74e3          improvement
    lengthTail      time            6.47e3          5.30e3          improvement
    lengthTailLazy  time            6.11e3          5.34e3          improvement


<a id="org01bde6f"></a>

## Space

     perf-explore -n 10 -l 100000 --space +RTS -T -RTS

     label1          label2          old result      new result      change
    
     sum             MaxMem          4.61e6          4.61e6
     sum             allocated       4.20e5          4.20e5
     sum             gcLiveBytes     2.15e5          2.17e5
     sum             gcollects       1.00e-1         1.00e-1
     sum             maxLiveBytes    0.00e0          0.00e0

Data is collected from GHCStats

-   allocated_bytes
-   gcs
-   gcdetails_live_bytes
-   max_live_bytes
-   max_mem_in_use_bytes


<a id="org753786d"></a>

# Perf.BigO

Perf.BigO represents functionality to determine the complexity order for a computation.

We could do a regression and minimise the error term, but we know that the largest run contains the most information; we would need to weight the simulations according to some heuristic.

Instead, we:

-   estimate the order factor for each possible Order, from N3 to N0, setting the highest n run constant factor to zero,
-   pick the order based on lowest absolute error result summed across all the runs,

    import qualified Prelude as P
    import Data.List (nub)
    estOrder (\x -> sum $ nub [1..x]) 10 [1,10,100,1000]

    BigOrder {bigOrder = N2, bigFactor = 4.05725, bigConstant = 0.0}


<a id="org47311bd"></a>

## Cache speed

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

