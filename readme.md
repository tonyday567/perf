[perf](https://tonyday567.github.io/perf/index.html)
====================================================

[![Build
Status](https://travis-ci.org/tonyday567/perf.svg)](https://travis-ci.org/tonyday567/perf)
[![Hackage](https://img.shields.io/hackage/v/perf.svg)](https://hackage.haskell.org/package/perf)
[![lts](https://www.stackage.org/package/perf/badge/lts)](http://stackage.org/lts/package/perf)
[![nightly](https://www.stackage.org/package/perf/badge/nightly)](http://stackage.org/nightly/package/perf)

[repo](https://github.com/tonyday567/perf)

Performance experiments using the
[rdtsc](https://en.wikipedia.org/wiki/Time_Stamp_Counter) register on
x86.

Benchmarks
==========

The code for these benchmark runs can be found in
[examples/examples.hs](examples/examples.hs).

Benchmarks are based on:

    number of runs:         1.00e3
    accumulate to:          1.00e3
    function:               foldl' (+) 0

1 cycle = 0.38 𝛈s (Based on my 2.6GHz machine, by definition).

tick\_
------

    one tick_: 16 cycles
    next 10: [14,16,16,16,16,16,16,16,16,16]
    average over 1m: 18.37 cycles
    99.999% perc: 20,841
    99.9% perc: 50.59
    99th perc:  24.72
    40th perc:  17.17
    [min, 10th, 20th, .. 90th, max]:
     12.00 15.28 15.93 16.58 17.17 17.67 18.17 18.67 19.38 20.49 3.656e4

tick
----

    sum to 1000
    first measure: 1020 cycles
    second measure: 16 cycles

ticks
-----

    sum to 1000 n = 1000 prime run: 1.02e3
    run                       first     2nd     3rd     4th     5th  40th %
    ticks                    7.80e3  1.42e3  1.33e3  1.33e3  1.34e3    688 cycles
    ticks (lambda)              868  1.36e3  1.32e3  1.32e3  1.32e3 1.32e3 cycles
    ticks (poly)             1.17e3     740     720     690     726    690 cycles
    ticksIO                  1.21e3     736     724     700     698    695 cycles
    ticksIO (lambda)            800     728     690     720     694    691 cycles
    ticksIO (poly)              910     710     740     692     722    695 cycles

ticks cost
----------

Looking for hidden computation costs:

    n =    1.00 outside:  7.88e4 inside:  2.58e4 gap:  5.30e4
    n =    10.0 outside:  7.66e4 inside:  3.61e4 gap:  4.04e4
    n =     100 outside:  1.59e5 inside:  1.19e5 gap:  4.02e4
    n =  1.00e3 outside:  7.40e5 inside:  6.99e5 gap:  4.14e4

tickns
------

Multiple runs summing to a series of numbers.

    sum to's [1,10,100,1000]
    ns (ticks n fMono) as:  23.9 33.0 113 689
    (replicateM n . tick fMono) <$> as:  15.7 17.1 92.0 673

vector
------

    sum to 1000
    ticks list               5.93e4  2.23e4  5.20e4  1.75e4  1.75e4 1.45e4 cycles
    ticks boxed              1.34e4  8.23e3  8.00e3  7.74e3  7.82e3 6.25e3 cycles
    ticks storable           5.17e3  1.53e3  1.46e3  1.45e3  1.45e3 1.45e3 cycles
    ticks unboxed            2.62e3  1.99e3  1.97e3  1.92e3  1.93e3 1.93e3 cycles

whnf
----

    sum to 1000
    tick                     2.96e3 cycles
    tickWHNF                    388 cycles
    ticks                    1.53e3  1.05e3  1.01e3  1.05e3  1.01e3 1.01e3 cycles
    ticksWHNF                  80.0    24.0    30.0    24.0    26.0   25.3 cycles
    tickIO                   1.05e3 cycles
    tickWHNFIO                 24.0 cycles
    ticksIO                  1.80e3  1.15e3  1.02e3  1.05e3  1.02e3 1.01e3 cycles
    ticksWHNFIO                 128    46.0    24.0    24.0    24.0   20.5 cycles

R&D, To Do
==========

metal speed
-----------

The average cycles per (+) operation can get down to 0.7, and there are
about 4 cache registers per cycle, so 2.8 low level instructions per
(+). Is this close to the metal speed?

unboxed versus boxed
--------------------

[ghc user
guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=inline#unboxed-type-kinds)

strictness
----------

[All about
strictness](https://www.fpcomplete.com/blog/2017/09/all-about-strictness)

sharing
-------

-   turn on no-full-laziness and no-cse
-   add the inline pragma to `ticks`

comparative results

-   no-full-laziness & no-cse in Perf.Cycle (mono function)

<!-- -->

    ticks non-memo 1.28e5
    ticks inline memo
    ticks noinline no-memo
    ticks inlinable non-memo
    ticksIO non-memo 6.48e4

    no-full-laziness in Perf.cycle

    ticks non-memo 6.51e4
    ticks inline memo
    ticksIO non-memo 1.28e5

    no-cse

    ticks memo 7.66e4
    ticks inline memo
    ticksIO nomemo 6.47e4

polymorphism
------------

Can slow a computation down by 2x

lambda expressions
------------------

Can really slow things down

workflow
========

    stack build --test --exec "$(stack path --local-install-root)/bin/perf-examples" --exec "$(stack path --local-bin)/pandoc -f markdown -i other/header.md examples/bench.md other/footer.md -t html -o index.html --filter pandoc-include --mathjax" --exec "$(stack path --local-bin)/pandoc -f markdown -i examples/bench.md -t markdown -o readme.md --filter pandoc-include --mathjax" --file-watch

solo experiments:

    stack exec "ghc" -- -O2 -rtsopts examples/summing.lhs
    ./examples/summing +RTS -s -RTS --runs 10000 --sumTo 1000 --chart --chartName other/sum1e3.svg --truncAt 4

references
==========

-   [rts cheat
    sheet](https://www.cheatography.com/nash/cheat-sheets/ghc-and-rts-options/)

-   [ghc tips](http://ghc.readthedocs.io/en/8.0.2/sooner.html)

time
----

-   [Optimising haskell for a tight inner
    loop](http://neilmitchell.blogspot.co.uk/2014/01/optimising-haskell-for-tight-inner-loop.html)

-   [Tools for analysing
    performance](http://stackoverflow.com/questions/3276240/tools-for-analyzing-performance-of-a-haskell-program/3276557#3276557)

-   [Write haskell as fast as
    c](https://donsbot.wordpress.com/2008/05/06/write-haskell-as-fast-as-c-exploiting-strictness-laziness-and-recursion/)

-   [Reading ghc
    core](http://stackoverflow.com/questions/6121146/reading-ghc-core)

space
-----

-   [Chasing space leaks in
    shake](http://neilmitchell.blogspot.com.au/2013/02/chasing-space-leak-in-shake.html)

-   [Space leak zoo](http://blog.ezyang.com/2011/05/space-leak-zoo/)

-   [Anatomy of a thunk
    leak](http://blog.ezyang.com/2011/05/anatomy-of-a-thunk-leak/)

-   [An insufficiently lazy
    map](http://blog.ezyang.com/2011/05/an-insufficiently-lazy-map/)

-   [Pinpointing space leaks in big
    programs](http://blog.ezyang.com/2011/06/pinpointing-space-leaks-in-big-programs/)

memoization
-----------

http://okmij.org/ftp/Haskell/\#memo-off

cache cycle estimates
---------------------

  Cache               Cycles
  ------------------- ----------------
  register            4 per cycle
  L1 Cache access     3-4 cycles
  L2 Cache access     11-12 cycles
  L3 unified access   30 - 40
  DRAM hit            195 cycles
  L1 miss             40 cycles
  L2 miss             &gt;600 cycles

A performance checklist
-----------------------

1.  compile with rtsopts flag

<!-- -->

    stack ghc -- --make examples/examples.hs -rtsopts -fforce-recomp

2.  check GC `examples/examples +RTS -s`

3.  enabling profiling

-   a normal ghc:
    `stack ghc -- --make examples/examples.hs -rtsopts -fforce-recomp`
-   profile enabled automatically:
    `stack ghc -- --make examples/examples.hs -rtsopts -fforce-recomp -prof -auto -auto-all`
-   if template haskell:
    `stack ghc -- --make examples/examples.hs -rtsopts -fforce-recomp -prof -auto -auto-all -osuf p_o`

4.  create an examples.prof on execution:
    `time examples/examples +RTS -p`

5.  space

<!-- -->

    examples/examples +RTS -p -hc
    hp2ps -e8in -c examples/examples.hp
    hp2ps -e8in -c examples/examples.hy # types
    hp2ps -e8in -c examples/examples.hp # constructors

6.  check strictness pragmas

7.  space leaks

<!-- -->

    examples/examples +RTS -s - additional memory
    examples/examples +RTS -xt -hy
