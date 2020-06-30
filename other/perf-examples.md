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
| pre warmup                  |                                                    22 |
| one tick\_                  |                                                    16 |
| next 10                     |                     \[18,18,16,18,18,16,16,20,22,18\] |
| average over one million    |                                                 16.93 |
| 99.999% perc                |                                                   244 |
| 99.9% perc                  |                                                 37.39 |
| 99th perc                   |                                                 20.68 |
| 40th perc                   |                                                 16.35 |
| \[min, 20th, .. 80th, max\] | 1.2000e1 1.5611e1 1.6352e1 1.7125e1 1.8129e1 5.5440e3 |

tick
----

sum to 1000

| stat           |                                                cycles |
|:---------------|------------------------------------------------------:|
| first measure  |                                                  1600 |
| second measure |                                                  1564 |
| third measure  |                                                  1408 |
| tick'          |                                                  1400 |
| tickIO         |                                                  1444 |
| tick \* 10     | \[1454,1404,1434,1518,1516,1518,1516,1518,1520,1524\] |
| tickIO \* 10   | \[2660,2672,2586,2586,2618,2614,2580,2612,2510,2188\] |
| tick' \* 10    | \[1344,1326,1426,1512,1512,1510,1510,1514,1514,1510\] |

ticks
-----

| run             |  first | second |  third | average | median |
|:----------------|-------:|-------:|-------:|--------:|-------:|
| monomorphic     | 3.41e3 | 2.91e3 | 2.87e3 |  2.86e3 | 2.83e3 |
| includes lambda | 2.83e3 | 2.84e3 | 2.83e3 |  2.85e3 | 2.84e3 |
| polymorphic     | 2.97e3 | 2.94e3 | 2.81e3 |  2.85e3 | 2.83e3 |
| ticksIO mono    | 8.02e3 | 2.93e3 | 2.89e3 |  2.97e3 | 2.96e3 |
| ticksIO lambda  | 2.93e3 | 2.59e3 | 2.56e3 |  2.54e3 | 2.58e3 |
| ticksIO poly    | 3.05e3 | 2.90e3 | 2.94e3 |  2.86e3 | 2.85e3 |

gaps
----

Looking for hidden computation costs:

| number runs | outside cycles |
|:------------|---------------:|
| 1.0e0       |        1.088e5 |
| 1.0e1       |        3.637e6 |
| 1.0e2       |        3.993e5 |
| 1.0e3       |        2.914e6 |

tickns
------

Multiple runs summing to a series of numbers.

| sum to:                              |       1 |
|:-------------------------------------|--------:|
| (replicateM n . tick fMono) \<$\> as | 2.164e1 |
| ns (ticks n fMono) as                | 2.481e1 |

vector
------

sum to 1000

| run            |  first | second |  third | average | median |
|:---------------|-------:|-------:|-------:|--------:|-------:|
| ticks list     | 2.76e4 | 1.66e4 | 1.52e6 |  2.55e4 | 2.80e4 |
| ticks boxed    | 1.56e4 | 1.52e4 | 1.42e4 |  1.49e4 | 1.52e4 |
| ticks storable | 1.18e4 | 1.14e4 | 1.04e4 |  1.12e4 | 1.13e4 |
| ticks unboxed  | 1.29e4 | 1.16e4 | 1.26e4 |  1.23e4 | 1.25e4 |

whnf
----

sum to 1000

| function    |  cycles |
|:------------|--------:|
| tick        | 1.566e3 |
| tickWHNF    | 1.568e3 |
| ticks       | 3.852e3 |
| ticksWHNF   | 8.000e1 |
| tickIO      | 1.564e3 |
| tickWHNFIO  | 2.200e1 |
| ticksIO     | 3.468e3 |
| ticksWHNFIO | 1.260e2 |

perf
----

perf cycle measurements

| effect          |  cycles |
|:----------------|--------:|
| file read       | 2.322e5 |
| length          | 2.998e4 |
| print to screen | 2.611e4 |
| sum             | 1.517e4 |
