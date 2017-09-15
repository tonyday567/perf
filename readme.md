[perf](https://tonyday567.github.io/perf/index.html)
===

[![Build Status](https://travis-ci.org/tonyday567/perf.svg)](https://travis-ci.org/tonyday567/perf) [![Hackage](https://img.shields.io/hackage/v/perf.svg)](https://hackage.haskell.org/package/perf) [![lts](https://www.stackage.org/package/perf/badge/lts)](http://stackage.org/lts/package/perf) [![nightly](https://www.stackage.org/package/perf/badge/nightly)](http://stackage.org/nightly/package/perf)

[repo](https://github.com/tonyday567/perf)

If you want to make stuff very fast in haskell, you need to dig down below the criterion-style level of abstraction and start counting cycles using the [rdtsc](https://en.wikipedia.org/wiki/Time_Stamp_Counter) register on x86.

This library is an experiment in measuring cycles (or ticks), and development of intuition about what is going on at the very fast level.

https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=inline#unboxed-type-kinds




Examples
===

The code for all results can be found in [examples/examples.hs](examples/examples.hs).


tick_
---

```include
other/tick_.md
```

tick
---

```include
other/tick.md
```

ticks
---

```include
other/ticks.md
```

ticks cost
---

Looking for hidden computation costs:

```include
other/ticksCost.md
```

tickns
---

Multiple runs summing to a series of numbers.

```include
other/tickns.md
```

vector
---

```include
other/vector.md
```

Findings
===

strictness
---

See https://www.fpcomplete.com/blog/2017/09/all-about-strictness

memoization
---

- turn on no-full-laziness and no-cse
- add the inline pragma to ticks


no-full-laziness & nocse in Perf.Cycle (mono function)

ticks non-memo 1.28e5
ticks inline memo
ticks noinline no-memo
ticks inlinable non-memo
ticksIO no-memo 6.48e4

no-full-laziness in Perf.cycle

ticks non-memo 6.51e4
ticks inline memo
ticksIO nomemo 1.28e5

no-cse

ticks memo 7.66e4
ticks inline memo
ticksIO nomemo 6.47e4





polymorphism
---

Can slow a computation down by 2x

lambda expressions
---

really slow things down 1.58e6 versus 6.42e4

workflow
===

```
stack build --test --exec "$(stack path --local-install-root)/bin/perf-examples" --exec "$(stack path --local-bin)/pandoc -f markdown -i other/header.md readme.md other/footer.md -t html -o index.html --filter pandoc-include --mathjax" --file-watch
```

solo experiments:

```
stack exec "ghc" -- -O2 -rtsopts examples/summing.lhs
./examples/summing +RTS -s -RTS --runs 10000 --sumTo 1000 --chart --chartName other/sum1e3.svg --truncAt 4
```


references
===

- [rts cheat sheet](https://www.cheatography.com/nash/cheat-sheets/ghc-and-rts-options/)

- [ghc tips](http://ghc.readthedocs.io/en/8.0.2/sooner.html)


time
---

- [Optimising haskell for a tight inner loop](http://neilmitchell.blogspot.co.uk/2014/01/optimising-haskell-for-tight-inner-loop.html)

- [Tools for analysing performance](http://stackoverflow.com/questions/3276240/tools-for-analyzing-performance-of-a-haskell-program/3276557#3276557)

- [Write haskell as fast as c](https://donsbot.wordpress.com/2008/05/06/write-haskell-as-fast-as-c-exploiting-strictness-laziness-and-recursion/)

- [Reading ghc core](http://stackoverflow.com/questions/6121146/reading-ghc-core)

space
---

- [Chasing space leaks in shake](http://neilmitchell.blogspot.com.au/2013/02/chasing-space-leak-in-shake.html)

- [Space leak zoo](http://blog.ezyang.com/2011/05/space-leak-zoo/)

- [Anatomy of a thunk leak](http://blog.ezyang.com/2011/05/anatomy-of-a-thunk-leak/)

- [An insufficiently lazy map](http://blog.ezyang.com/2011/05/an-insufficiently-lazy-map/)

- [Pinpointing space leaks in big programs](http://blog.ezyang.com/2011/06/pinpointing-space-leaks-in-big-programs/)

memoization
---

http://okmij.org/ftp/Haskell/#memo-off


cache cycle estimates
---------------------

| Cache             | Cycles         |
|-------------------|----------------|
| register          | 4 per cycle    |
| L1 Cache access   | 3-4 cycles     |
| L2 Cache access   | 11-12 cycles   |
| L3 unified access | 30 - 40        |
| DRAM hit          | 195 cycles     |
| L1 miss           | 40 cycles      |
| L2 miss           | &gt;600 cycles |

A performance checklist
---

1. compile with rtsopts flag

```
stack ghc -- --make examples/examples.hs -rtsopts -fforce-recomp
```

2.  check GC `examples/examples +RTS -s`

3.  enabling profiling

  - a normal ghc: `stack ghc -- --make examples/examples.hs -rtsopts -fforce-recomp`
  - profile enabled automatically: `stack ghc -- --make examples/examples.hs -rtsopts -fforce-recomp -prof -auto -auto-all`
  - if template haskell: `stack ghc -- --make examples/examples.hs -rtsopts -fforce-recomp -prof -auto -auto-all -osuf p_o`

4.  create an examples.prof on execution: `time examples/examples +RTS -p`

5.  space

```
examples/examples +RTS -p -hc
hp2ps -e8in -c examples/examples.hp
hp2ps -e8in -c examples/examples.hy # types
hp2ps -e8in -c examples/examples.hp # constructors
```

6.  check strictness pragmas

7.  space leaks

```
examples/examples +RTS -s - additional memory
examples/examples +RTS -xt -hy
```
