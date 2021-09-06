perf
===

[![Hackage](https://img.shields.io/hackage/v/perf.svg)](https://hackage.haskell.org/package/perf)
[![Build Status](https://github.com/tonyday567/perf/workflows/haskell-ci/badge.svg)](https://github.com/tonyday567/perf/actions?query=workflow%3Ahaskell-ci) [![Hackage Deps](https://img.shields.io/hackage-deps/v/perf.svg)](http://packdeps.haskellers.com/reverse/perf)

Low-level performance measurement for haskell using the [rdtsc](https://en.wikipedia.org/wiki/Time_Stamp_Counter) register on x86.

metal speed
---

The average cycles per (+) operation can get down to 0.7, and there are about 4 cache registers per cycle, so 2.8 low level instructions per (+).  Is this close to the metal speed?

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

Profiling
---

https://www.tweag.io/blog/2020-01-30-haskell-profiling/

hp2pretty

threadscope

https://stackoverflow.com/questions/32123475/profiling-builds-with-stack


https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html

```
mkdir -p .stack-bin
stack clean
stack install --work-dir .stack-work-profile --profile --local-bin-path .stack-bin --ghc-options=-fno-prof-auto
stack exec --profile -- example +RTS -p -hc -s
stack exec -- hp2ps -c example.hp && ps2pdf example.ps
stack exec ghc-prof-flamegraph -- example.prof
```

memory checks

```
hp2ps -e8in -c examples.hp
hp2ps -e8in -c examples.hy # types
hp2ps -e8in -c examples.hp # constructors
stack exec example -- +RTS -s - additional memory
stack exec example -- +RTS -xt -hy
```

