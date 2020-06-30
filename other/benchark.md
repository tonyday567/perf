benchmarks
==========

All measurements in cycles. 1 cycle = 0.38 𝛈s (Based on my 2.6GHz
machine, by definition).

run
---

|                |              |
|:---------------|-------------:|
| number of runs |       1.00e3 |
| accumulate to  |       1.00e3 |
| function       | foldl' (+) 0 |

tick callibration
-----------------

| stat                        |                                                cycles |
|:----------------------------|------------------------------------------------------:|
| pre warmup                  |                                                    16 |
| one tick\_                  |                                                    16 |
| next 10                     |                     \[16,16,16,16,18,18,18,16,16,14\] |
| average over one million    |                                                 16.64 |
| 99.999% perc                |                                                   462 |
| 99.9% perc                  |                                                 41.36 |
| 99th perc                   |                                                 19.00 |
| 40th perc                   |                                                 15.86 |
| \[min, 20th, .. 80th, max\] | 1.2000e1 1.5224e1 1.5861e1 1.6498e1 1.7363e1 7.7112e4 |

tick
----

sum to 1000

| stat           |                                                cycles |
|:---------------|------------------------------------------------------:|
| first measure  |                                                  1566 |
| second measure |                                                  1570 |
| third measure  |                                                  1432 |
| tick'          |                                                  1426 |
| tickIO         |                                                  1434 |
| tick \* 10     | \[2370,2326,2608,2584,2578,2616,2574,2614,2574,2614\] |
| tickIO \* 10   | \[2882,2844,2788,2794,2512,2210,2156,2500,2778,2800\] |
| tick' \* 10    | \[2618,2578,2622,2624,2622,2580,2620,2578,2620,2626\] |

ticks
-----

| run             |  first | second |  third | average | median |
|:----------------|-------:|-------:|-------:|--------:|-------:|
| monomorphic     | 3.39e3 | 2.86e3 | 2.92e3 |  2.86e3 | 2.84e3 |
| includes lambda | 1.97e3 | 1.96e3 | 1.96e3 |  2.45e3 | 1.96e3 |
| polymorphic     | 2.83e3 | 2.82e3 | 2.83e3 |  2.69e3 | 2.84e3 |
| ticksIO mono    | 2.08e3 | 1.40e3 | 1.38e3 |  1.36e3 | 1.35e3 |
| ticksIO lambda  | 2.12e3 | 2.00e3 | 1.97e3 |  1.97e3 | 1.97e3 |
| ticksIO poly    | 2.15e3 | 2.00e3 | 2.00e3 |  1.97e3 | 1.97e3 |

gaps
----

Looking for hidden computation costs:

| number runs | outside cycles |
|:------------|---------------:|
| 1.0e0       |        6.784e4 |
| 1.0e1       |        1.987e6 |
| 1.0e2       |        2.693e5 |
| 1.0e3       |        2.003e6 |

tickns
------

Multiple runs summing to a series of numbers.

| sum to:                              |       1 |
|:-------------------------------------|--------:|
| (replicateM n . tick fMono) \<$\> as | 1.606e1 |
| ns (ticks n fMono) as                | 2.774e1 |

vector
------

sum to 1000

| run            |  first | second |  third | average | median |
|:---------------|-------:|-------:|-------:|--------:|-------:|
| ticks list     | 2.67e4 | 1.71e4 | 1.53e6 |  1.91e4 | 1.48e4 |
| ticks boxed    | 7.58e3 | 7.18e3 | 7.16e3 |  7.16e3 | 7.12e3 |
| ticks storable | 6.77e3 | 6.18e3 | 6.17e3 |  7.30e3 | 6.16e3 |
| ticks unboxed  | 1.31e4 | 1.27e4 | 1.26e4 |  7.75e3 | 7.01e3 |

whnf
----

sum to 1000

| function    |  cycles |
|:------------|--------:|
| tick        | 7.120e2 |
| tickWHNF    | 7.340e2 |
| ticks       | 3.196e3 |
| ticksWHNF   | 6.000e1 |
| tickIO      | 7.240e2 |
| tickWHNFIO  | 1.800e1 |
| ticksIO     | 2.478e3 |
| ticksWHNFIO | 7.800e1 |

perf
----

perf cycle measurements

| effect          |  cycles |
|:----------------|--------:|
| file read       | 1.902e5 |
| length          | 2.614e4 |
| print to screen | 2.690e4 |
| sum             | 1.256e4 |
