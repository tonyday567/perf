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
| pre warmup                  | 32                                                    |
| one tick\_                  | 16                                                    |
| next 10                     | \[16,14,14,14,14,14,16,16,14,14\]                     |
| average over one million    | 16.26                                                 |
| 99.999% perc                | 249                                                   |
| 99.9% perc                  | 40.20                                                 |
| 99th perc                   | 18.96                                                 |
| 40th perc                   | 15.77                                                 |
| \[min, 20th, .. 80th, max\] | 1.2000e1 1.5151e1 1.5765e1 1.6380e1 1.6995e1 4.1422e4 |

tick
----

sum to 1000

| stat           | cycles                                                |
|----------------|-------------------------------------------------------|
| first measure  | 1430                                                  |
| second measure | 1378                                                  |
| third measure  | 1448                                                  |
| tick'          | 1360                                                  |
| tickIO         | 2626                                                  |
| tick \* 10     | \[1346,1374,1436,1390,1338,1328,1330,1322,1326,1326\] |
| tickIO \* 10   | \[1394,1386,1374,1340,1316,1330,1322,1334,1322,1324\] |
| tick' \* 10    | \[1360,1348,1358,1332,1318,1328,1326,1326,1392,1340\] |

ticks
-----

| run             |   first|  second|   third|  average|  median|
|:----------------|-------:|-------:|-------:|--------:|-------:|
| monomorphic     |  2.77e3|  2.67e3|  2.64e3|   2.47e3|  2.46e3|
| includes lambda |  2.63e3|  2.43e3|  2.36e3|   2.47e3|  2.46e3|
| polymorphic     |  2.64e3|  2.64e3|  2.44e3|   2.47e3|  2.46e3|
| ticksIO mono    |  3.32e3|  3.14e3|  2.89e3|   2.85e3|  2.85e3|
| ticksIO lambda  |  2.76e3|  2.48e3|  2.46e3|   2.54e3|  2.53e3|
| ticksIO poly    |  2.93e3|  2.61e3|  2.51e3|   2.51e3|  2.51e3|

gaps
----

Looking for hidden computation costs:

| number runs | outside cycles | inside cycles | gap     |
|-------------|----------------|---------------|---------|
| 1.0e0       | 1.069e5        | 4.331e4       | 6.360e4 |
| 1.0e1       | 2.772e6        | 2.691e6       | 8.013e4 |
| 1.0e2       | 3.685e5        | 3.016e5       | 6.686e4 |
| 1.0e3       | 2.535e6        | 2.466e6       | 6.888e4 |

tickns
------

Multiple runs summing to a series of numbers.

| sum to:                              | 1       | 10      | 100     | 1000    |
|--------------------------------------|---------|---------|---------|---------|
| (replicateM n . tick fMono) \<$\> as | 1.736e1 | 3.760e1 | 2.435e2 | 1.311e3 |
| ns (ticks n fMono) as                | 2.587e1 | 3.922e1 | 1.855e2 | 1.394e3 |

vector
------

sum to 1000

| run            |   first|  second|   third|  average|  median|
|:---------------|-------:|-------:|-------:|--------:|-------:|
| ticks list     |  2.55e4|  1.63e4|  1.37e6|   2.61e4|  2.44e4|
| ticks boxed    |  7.04e3|  6.55e3|  6.53e3|   6.92e3|  6.48e3|
| ticks storable |  2.43e3|  2.01e3|  1.97e3|   1.97e3|  1.97e3|
| ticks unboxed  |  2.19e3|  1.80e3|  1.73e3|   1.75e3|  1.75e3|

whnf
----

sum to 1000

| function    | cycles  |         |         |         |         |
|-------------|---------|---------|---------|---------|---------|
| tick        | 6.960e2 |         |         |         |         |
| tickWHNF    | 8.580e2 |         |         |         |         |
| ticks       | 2.168e3 | 1.492e3 | 1.438e3 | 1.375e3 | 1.384e3 |
| ticksWHNF   | 5.420e2 | 1.800e1 | 2.000e1 | 1.946e1 | 1.803e1 |
| tickIO      | 7.780e2 |         |         |         |         |
| tickWHNFIO  | 1.600e1 |         |         |         |         |
| ticksIO     | 2.346e3 | 1.984e3 | 1.988e3 | 2.061e3 | 1.968e3 |
| ticksWHNFIO | 7.600e1 | 2.000e1 | 1.800e1 | 2.029e1 | 1.859e1 |

perf
----

perf cycle measurements

| effect          | cycles  |
|-----------------|---------|
| file read       | 2.127e5 |
| length          | 1.979e4 |
| print to screen | 1.891e4 |
| sum             | 1.296e4 |

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
