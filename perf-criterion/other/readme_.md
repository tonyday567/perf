perf-criterion
===

[![Build Status](https://travis-ci.org/tonyday567/perf-criterion.svg)](https://travis-ci.org/tonyday567/perf-criterion) [![Hackage](https://img.shields.io/hackage/v/perf-criterion.svg)](https://hackage.haskell.org/package/perf-criterion) [![lts](https://www.stackage.org/package/perf-criterion/badge/lts)](http://stackage.org/lts/package/perf-criterion) [![nightly](https://www.stackage.org/package/perf-criterion/badge/nightly)](http://stackage.org/nightly/package/perf-criterion) 

```
stack build --test --exec "$(stack path --local-install-root)/bin/perf-criterion" --exec "$(stack path --local-bin)/pandoc -f markdown+lhs -i app/perf-criterion.lhs -t markdown -o readme.md --filter pandoc-include --mathjax" -- exec "vmd readme.md" --file-watch
```
