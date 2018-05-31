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

    stack build --test --exec "$(stack path --local-install-root)/bin/perf-examples" --exec "$(stack path --local-bin)/pandoc -f markdown -i other/readme_.md -t markdown -o readme.md --filter pandoc-include --mathjax" --exec "vmd readme.md"

benchmarks
==========

All measurements in cycles. 1 cycle = 0.38 𝛈s (Based on my 2.6GHz
machine, by definition).

run
---

    number of runs:           1.00e3
    accumulate to:            1.00e3
    function:               foldl' (+) 0

tick callibration
-----------------

    pre warmup: 88 cycles
    one tick_: 46 cycles
    next 10: [16,14,16,16,16,16,16,16,16,16]
    average over 1m: 16.22 cycles
    99.999% perc: 206
    99.9% perc: 48.39
    99th perc:  18.95
    40th perc:  15.74
    [min, 20th, .. 80th, max]:
     1.2000e1 1.5129e1 1.5737e1 1.6346e1 1.6955e1 1.5300e4

tick
----

    sum to 1000
    first measure: 968 cycles
    second measure: 1520 cycles
    third measure: 912 cycles
    tick': 906 cycles
    tickIO: 1140 cycles
    tick * 10: [736,676,674,672,698,674,702,674,698,672]
    tickIO * 10: [736,688,676,672,718,702,696,676,702,674]
    tick * 10: [674,720,674,700,674,698,672,696,670,702]

ticks
-----

    acc = 1000 n = 1000
    run                        first      2nd      3rd   median      av.
    monomorphic               2.20e3   1.45e3   1.42e3   1.41e3   1.41e3
    includes lambda           1.40e3   1.37e3   1.32e3   1.32e3   1.33e3
    polymorphic               1.37e3   1.40e3   1.41e3   1.40e3   1.58e3
    ticksIO mono              1.90e3   1.45e3   1.37e3   1.35e3   1.36e3
    ticksIO lambda            1.49e3   1.48e3   1.33e3   1.39e3   1.38e3
    ticksIO poly              8.29e3   1.40e3   1.33e3   1.36e3   1.36e3

ticks cost
----------

Looking for hidden computation costs:

    n = 1.000e0 outside: 6.235e4 inside: 2.385e4 gap: 3.850e4
    n = 1.000e1 outside: 7.522e4 inside: 3.857e4 gap: 3.665e4
    n = 1.000e2 outside: 2.122e5 inside: 1.753e5 gap: 3.687e4
    n = 1.000e3 outside: 1.369e6 inside: 1.331e6 gap: 3.802e4

tickns
------

Multiple runs summing to a series of numbers.

    sum to's [1,10,100,1000]
    ns (ticks n fMono) as:  2.289e1 3.803e1 1.715e2 1.326e3
    (replicateM n . tick fMono) <$> as:  1.571e1 2.229e1 1.494e2 1.303e3

vector
------

    sum to 1000
    ticks list                2.74e4   1.58e4   1.57e4   1.30e4   1.35e4
    ticks boxed               6.47e3   5.91e3   5.92e3   6.01e3   5.99e3
    ticks storable            1.83e3   1.99e3   1.96e3   1.96e3   2.06e3
    ticks unboxed             2.16e3   1.83e3   1.80e3   1.97e3   1.96e3

whnf
----

    sum to 1000
    tick                      1.62e3
    tickWHNF                  1.83e3
    ticks                     3.21e3   1.99e3   1.66e3   1.40e3   1.41e3
    ticksWHNF                 5.60e1   1.80e1   1.80e1   1.75e1   1.83e1
    tickIO                    9.80e2
    tickWHNFIO                1.80e1
    ticksIO                   1.81e3   1.45e3   1.38e3   1.40e3   1.49e3
    ticksWHNFIO               1.08e2   2.00e1   2.00e1   1.67e1   1.83e1

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

8.  read core

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
