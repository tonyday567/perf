`replicate . ticks` seems to memo'ize

| run         |  cputime |  cycles |
|:------------|---------:|--------:|
| fib 1 (ns)  | 7.326e-8 | 1.928e2 |
| fib 5 (ns)  | 3.580e-8 | 9.420e1 |
| fib 9 (ns)  | 5.381e-8 | 1.416e2 |
| fib 11 (ns) | 7.509e-8 | 1.976e2 |

perf runs

| run         |  cputime |  cycles |
|:------------|---------:|--------:|
| fib 1 (ns)  | 1.094e-8 | 2.879e1 |
| fib 5 (ns)  | 3.033e-8 | 7.981e1 |
| fib 9 (ns)  | 1.924e-7 | 5.062e2 |
| fib 11 (ns) | 4.906e-7 | 1.291e3 |

criterion runs

| run         | cputimes |     cycles |
|:------------|---------:|-----------:|
| fib 1 (ns)  | 1.800e-8 | -1.798e308 |
| fib 5 (ns)  | 7.800e-8 | -1.798e308 |
| fib 9 (ns)  | 2.410e-7 | -1.798e308 |
| fib 11 (ns) | 5.680e-7 | -1.798e308 |
