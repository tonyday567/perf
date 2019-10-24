perf
====

[![Build
Status](https://travis-ci.org/tonyday567/perf.svg)](https://travis-ci.org/tonyday567/perf)

Low-level performance measurement for haskell using the
[rdtsc](https://en.wikipedia.org/wiki/Time_Stamp_Counter) register on
x86.

libraries
=========

perf
----

Core functionality.

[![Hackage](https://img.shields.io/hackage/v/perf.svg)](https://hackage.haskell.org/package/perf)
[![lts](https://www.stackage.org/package/perf/badge/lts)](http://stackage.org/lts/package/perf)
[![nightly](https://www.stackage.org/package/perf/badge/nightly)](http://stackage.org/nightly/package/perf)

perf-analysis
-------------

Analysis using perf. Code for the benchmark runs can be found in
[perf-analysis/examples/examples.hs](perf-analysis/examples/examples.hs).
To create this readme locally run:

    stack build --test --exec "$(stack path --local-install-root)/bin/perf-examples" --file-watch

perf-criterion
--------------

Comparison with criterion. See [criterion.md](criterion.md). To create
this locally run:

    stack build --test --exec "$(stack path --local-install-root)/bin/perf-criterion" --file-watch

benchmarks
==========

All measurements in cycles. 1 cycle = 0.38 𝛈s (Based on my 2.6GHz
machine, by definition).

run
---

|                |              |
|:---------------|-------------:|
| number of runs |        1.00e3|
| accumulate to  |        1.00e3|
| function       |  foldl' (+) 0|

tick callibration
-----------------

| stat                        | cycles                                                |
|-----------------------------|-------------------------------------------------------|
| pre warmup                  | 150                                                   |
| one tick\_                  | 50                                                    |
| next 10                     | \[16,22,20,18,18,18,22,22,20,18\]                     |
| average over one million    | 16.79                                                 |
| 99.999% perc                | 15,533                                                |
| 99.9% perc                  | 48.41                                                 |
| 99th perc                   | 20.65                                                 |
| 40th perc                   | 15.87                                                 |
| \[min, 20th, .. 80th, max\] | 1.2000e1 1.5224e1 1.5865e1 1.6506e1 1.7412e1 8.2632e4 |

tick
----

sum to 1000

| stat           | cycles                                                |
|----------------|-------------------------------------------------------|
| first measure  | 3686                                                  |
| second measure | 1508                                                  |
| third measure  | 748                                                   |
| tick'          | 732                                                   |
| tickIO         | 1196                                                  |
| tick \* 10     | \[1384,1348,1310,1310,1312,1314,1312,1308,1308,1308\] |
| tickIO \* 10   | \[1398,1336,1308,1334,1306,1308,1306,1304,1308,1308\] |
| tick' \* 10    | \[1308,1304,1312,1304,1302,1302,1302,1310,1304,1302\] |

ticks
-----

| run             |   first|  second|   third|  average|  median|
|:----------------|-------:|-------:|-------:|--------:|-------:|
| monomorphic     |  2.93e3|  1.98e3|  1.95e3|   2.07e3|  1.96e3|
| includes lambda |  1.96e3|  1.96e3|  1.96e3|   1.96e3|  1.96e3|
| polymorphic     |  1.96e3|  1.96e3|  1.96e3|   1.97e3|  1.96e3|
| ticksIO mono    |  2.66e3|  2.03e3|  1.99e3|   1.96e3|  1.96e3|
| ticksIO lambda  |  1.14e4|  1.99e3|  1.99e3|   1.97e3|  1.96e3|
| ticksIO poly    |  1.52e3|  1.38e3|  1.35e3|   1.38e3|  1.38e3|

gaps
----

Looking for hidden computation costs:

| number runs | outside cycles | inside cycles | gap     |
|-------------|----------------|---------------|---------|
| 1.0e0       | 6.552e4        | 2.521e4       | 4.031e4 |
| 1.0e1       | 1.705e6        | 1.637e6       | 6.752e4 |
| 1.0e2       | 2.636e5        | 2.246e5       | 3.902e4 |
| 1.0e3       | 1.997e6        | 1.956e6       | 4.082e4 |

tickns
------

Multiple runs summing to a series of numbers.

| sum to:                              | 1       | 10      | 100     | 1000    |
|--------------------------------------|---------|---------|---------|---------|
| (replicateM n . tick fMono) \<$\> as | 1.601e1 | 1.854e1 | 1.526e2 | 1.307e3 |
| ns (ticks n fMono) as                | 2.327e1 | 4.282e1 | 2.252e2 | 1.957e3 |

vector
------

sum to 1000

| run            |   first|  second|   third|  average|  median|
|:---------------|-------:|-------:|-------:|--------:|-------:|
| ticks list     |  2.45e4|  1.78e4|  1.40e6|   1.65e4|  1.42e4|
| ticks boxed    |  6.51e3|  5.93e3|  5.89e3|   5.97e3|  5.99e3|
| ticks storable |  2.93e3|  2.64e3|  2.64e3|   2.64e3|  2.60e3|
| ticks unboxed  |  2.16e3|  1.80e3|  1.78e3|   1.96e3|  1.96e3|

whnf
----

sum to 1000

| function    | cycles  |         |         |         |         |
|-------------|---------|---------|---------|---------|---------|
| tick        | 8.680e2 |         |         |         |         |
| tickWHNF    | 1.126e3 |         |         |         |         |
| ticks       | 2.864e3 | 1.996e3 | 2.004e3 | 1.968e3 | 1.965e3 |
| ticksWHNF   | 6.200e1 | 1.800e1 | 1.800e1 | 1.825e1 | 1.741e1 |
| tickIO      | 9.540e2 |         |         |         |         |
| tickWHNFIO  | 1.600e1 |         |         |         |         |
| ticksIO     | 1.636e3 | 1.354e3 | 1.328e3 | 1.381e3 | 1.381e3 |
| ticksWHNFIO | 7.800e1 | 2.000e1 | 1.800e1 | 1.807e1 | 1.687e1 |

perf
----

perf cycle measurements

| effect          | cycles  |
|-----------------|---------|
| file read       | 2.455e5 |
| length          | 1.607e4 |
| print to screen | 3.723e4 |
| sum             | 1.072e4 |

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

solo experiment recipe:
-----------------------

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

    stack build --profile --executable-profiling --library-profiling

    stack exec -- example +RTS -p

    ./mytest +RTS -M4m -RTS

-   <a href="https://github.com/ndmitchell/spaceleak" class="uri">https://github.com/ndmitchell/spaceleak</a>

-   <a href="https://stackoverflow.com/questions/42353661/may-i-limit-memory-usage-per-function-monad-thread-in-haskell" class="uri">https://stackoverflow.com/questions/42353661/may-i-limit-memory-usage-per-function-monad-thread-in-haskell</a>

-   [Anatomy of a thunk
    leak](http://blog.ezyang.com/2011/05/anatomy-of-a-thunk-leak/)

memoization
-----------

<a href="http://okmij.org/ftp/Haskell/#memo-off" class="uri">http://okmij.org/ftp/Haskell/#memo-off</a>

cache cycle estimates
---------------------

| Cache             | Cycles       |
|-------------------|--------------|
| register          | 4 per cycle  |
| L1 Cache access   | 3-4 cycles   |
| L2 Cache access   | 11-12 cycles |
| L3 unified access | 30 - 40      |
| DRAM hit          | 195 cycles   |
| L1 miss           | 40 cycles    |
| L2 miss           | \>600 cycles |

A performance checklist
-----------------------

1.  compile with rtsopts flag

<!-- -->

    stack ghc -- --make examples/examples.hs -rtsopts -fforce-recomp

1.  check GC `examples/examples +RTS -s`

2.  enabling profiling

-   a normal ghc:
    `stack ghc -- --make examples/examples.hs -rtsopts -fforce-recomp`
-   profile enabled automatically:
    `stack ghc -- --make examples/examples.hs -rtsopts -fforce-recomp -prof -auto -auto-all`
-   if template haskell:
    `stack ghc -- --make examples/examples.hs -rtsopts -fforce-recomp -prof -auto -auto-all -osuf p_o`

1.  create an examples.prof on execution:
    `time examples/examples +RTS -p`

2.  space

<!-- -->

    examples/examples +RTS -p -hc
    hp2ps -e8in -c examples/examples.hp
    hp2ps -e8in -c examples/examples.hy # types
    hp2ps -e8in -c examples/examples.hp # constructors

1.  check strictness pragmas

2.  space leaks

<!-- -->

    examples/examples +RTS -s - additional memory
    examples/examples +RTS -xt -hy

1.  read core

<!-- -->

        stack exec ghc-core -- --no-cast --no-asm --no-syntax examples/simplest.hs >> other/simplest.core

        alias ghci-core="stack ghci -- -ddump-simpl -dsuppress-idinfo -dsuppress-coercions -dsuppress-type-applications -dsuppress-uniques -dsuppress-module-prefixes"

simplest core dump (cleaned up)

    ==================== Tidy Core ====================
    Result size of Tidy Core = {terms: 22, types: 31, coercions: 9}

    -- RHS size: {terms: 2, types: 0, coercions: 0}
    $trModule2 :: TrName
    $trModule2 = TrNameS "main"#

    -- RHS size: {terms: 2, types: 0, coercions: 0}
    $trModule1 :: TrName
    $trModule1 = TrNameS "Main"#

    -- RHS size: {terms: 3, types: 0, coercions: 0}
    $trModule :: Module
    $trModule = Module $trModule2 $trModule1

    -- RHS size: {terms: 4, types: 7, coercions: 0}
    main1
      :: State# RealWorld -> (# State# RealWorld, () #)
    main1 =
      \ (s_a1o2 [OS=OneShot] :: State# RealWorld) ->
        (# s_a1o2, () #)

    -- RHS size: {terms: 1, types: 0, coercions: 3}
    main :: IO ()
    main = main1 `cast` ...

    -- RHS size: {terms: 2, types: 1, coercions: 3}
    main2
      :: State# RealWorld -> (# State# RealWorld, () #)
    main2 = runMainIO1 @ () (main1 `cast` ...)

    -- RHS size: {terms: 1, types: 0, coercions: 3}
    :main :: IO ()
    :main = main2 `cast` ...

simplest core dump (full)

    [1 of 1] Compiling Main             ( examples/simplest.hs, examples/simplest.o )

    ==================== Tidy Core ====================
    Result size of Tidy Core = {terms: 22, types: 31, coercions: 9}

    -- RHS size: {terms: 2, types: 0, coercions: 0}
    $trModule2 :: TrName
    [GblId,

     Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
             WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 30 20}]
    $trModule2 = TrNameS "main"#

    -- RHS size: {terms: 2, types: 0, coercions: 0}
    $trModule1 :: TrName
    [GblId,

     Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
             WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 30 20}]
    $trModule1 = TrNameS "Main"#

    -- RHS size: {terms: 3, types: 0, coercions: 0}
    $trModule :: Module
    [GblId,

     Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
             WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 10 30}]
    $trModule = Module $trModule2 $trModule1

    -- RHS size: {terms: 4, types: 7, coercions: 0}
    main1
      :: State# RealWorld -> (# State# RealWorld, () #)
    [GblId,
     Arity=1,

     Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
             WorkFree=True, Expandable=True,
             Guidance=ALWAYS_IF(arity=1,unsat_ok=True,boring_ok=False)
             Tmpl= \ (s_a1o2 [Occ=Once, OS=OneShot]
                        :: State# RealWorld) ->
                     (# s_a1o2, () #)}]
    main1 =
      \ (s_a1o2 [OS=OneShot] :: State# RealWorld) ->
        (# s_a1o2, () #)

    -- RHS size: {terms: 1, types: 0, coercions: 3}
    main :: IO ()
    [GblId,
     Arity=1,

     Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
             WorkFree=True, Expandable=True,
             Guidance=ALWAYS_IF(arity=0,unsat_ok=True,boring_ok=True)
             Tmpl= main1 `cast` ...}]
    main = main1 `cast` ...

    -- RHS size: {terms: 2, types: 1, coercions: 3}
    main2
      :: State# RealWorld -> (# State# RealWorld, () #)
    [GblId,
     Arity=1,

     Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
             WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 20 60}]
    main2 = runMainIO1 @ () (main1 `cast` ...)

    -- RHS size: {terms: 1, types: 0, coercions: 3}
    :main :: IO ()
    [GblId,
     Arity=1,

     Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
             WorkFree=True, Expandable=True,
             Guidance=ALWAYS_IF(arity=0,unsat_ok=True,boring_ok=True)
             Tmpl= main2 `cast` ...}]
    :main = main2 `cast` ...

    Linking examples/simplest ...