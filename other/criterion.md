`replicate . ticks` seems to memo'ize

| run         |  cputime |  cycles |
|:------------|---------:|--------:|
| fib 1 (ns)  | 5.449e-8 | 1.434e2 |
| fib 5 (ns)  | 2.668e-8 | 7.020e1 |
| fib 9 (ns)  | 4.796e-8 | 1.262e2 |
| fib 11 (ns) | 7.273e-8 | 1.914e2 |

perf runs

| run         |  cputime |  cycles |
|:------------|---------:|--------:|
| fib 1 (ns)  | 1.111e-8 | 2.923e1 |
| fib 5 (ns)  | 2.909e-8 | 7.656e1 |
| fib 9 (ns)  | 1.958e-7 | 5.154e2 |
| fib 11 (ns) | 5.031e-7 | 1.324e3 |

criterion runs

| run         | cputimes |     cycles |
|:------------|---------:|-----------:|
| fib 1 (ns)  | 2.200e-8 | -1.798e308 |
| fib 5 (ns)  | 4.000e-8 | -1.798e308 |
| fib 9 (ns)  | 2.300e-7 | -1.798e308 |
| fib 11 (ns) | 5.790e-7 | -1.798e308 |
