# aoc2020

## Run

```
stack run
```

## Test

```
stack test
```

## Test Watch

```
stack build --file-watch --test --test-arguments '--rerun --failure-report=TESTREPORT --rerun-all-on-success'
```

## Benchmark

```
stack bench --benchmark-arguments '--output=benchmark.html'
```

## Benchmark results

### [Fancy HTML version](https://wamaral.github.io/advent-of-code/2020/benchmark.html)

> Note that not much effort was put into performance, solutions might be far from ideal in that regard

```
benchmarking Day 1/part 1
time                 507.8 μs   (490.3 μs .. 530.3 μs)
                     0.991 R²   (0.985 R² .. 0.996 R²)
mean                 527.7 μs   (519.9 μs .. 541.3 μs)
std dev              37.21 μs   (30.54 μs .. 47.54 μs)
variance introduced by outliers: 62% (severely inflated)

benchmarking Day 1/part 2
time                 97.67 ms   (96.19 ms .. 99.03 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 97.42 ms   (96.82 ms .. 97.92 ms)
std dev              861.9 μs   (500.6 μs .. 1.208 ms)

benchmarking Day 2/part 1
time                 5.779 ms   (5.635 ms .. 5.925 ms)
                     0.997 R²   (0.996 R² .. 0.999 R²)
mean                 5.710 ms   (5.661 ms .. 5.791 ms)
std dev              178.3 μs   (130.8 μs .. 283.9 μs)
variance introduced by outliers: 12% (moderately inflated)

benchmarking Day 2/part 2
time                 6.046 ms   (5.919 ms .. 6.182 ms)
                     0.996 R²   (0.993 R² .. 0.999 R²)
mean                 6.274 ms   (6.217 ms .. 6.363 ms)
std dev              229.6 μs   (175.4 μs .. 326.7 μs)
variance introduced by outliers: 15% (moderately inflated)

benchmarking Day 3/part 1
time                 2.241 ms   (2.185 ms .. 2.315 ms)
                     0.995 R²   (0.992 R² .. 0.999 R²)
mean                 2.196 ms   (2.176 ms .. 2.227 ms)
std dev              92.79 μs   (67.00 μs .. 134.1 μs)
variance introduced by outliers: 28% (moderately inflated)

benchmarking Day 3/part 2
time                 5.846 ms   (5.684 ms .. 6.065 ms)
                     0.993 R²   (0.988 R² .. 0.997 R²)
mean                 5.531 ms   (5.446 ms .. 5.637 ms)
std dev              294.7 μs   (233.4 μs .. 370.7 μs)
variance introduced by outliers: 29% (moderately inflated)

benchmarking Day 4/part 1
time                 4.293 ms   (4.118 ms .. 4.508 ms)
                     0.972 R²   (0.955 R² .. 0.983 R²)
mean                 4.453 ms   (4.319 ms .. 4.612 ms)
std dev              457.6 μs   (390.9 μs .. 564.5 μs)
variance introduced by outliers: 64% (severely inflated)

benchmarking Day 4/part 2
time                 5.838 ms   (5.200 ms .. 6.465 ms)
                     0.911 R²   (0.857 R² .. 0.955 R²)
mean                 7.092 ms   (6.671 ms .. 7.405 ms)
std dev              1.140 ms   (923.6 μs .. 1.561 ms)
variance introduced by outliers: 79% (severely inflated)

benchmarking Day 5/part 1
time                 1.218 ms   (1.134 ms .. 1.330 ms)
                     0.962 R²   (0.946 R² .. 0.982 R²)
mean                 1.256 ms   (1.211 ms .. 1.292 ms)
std dev              140.3 μs   (116.5 μs .. 163.4 μs)
variance introduced by outliers: 76% (severely inflated)

benchmarking Day 5/part 2
time                 1.623 ms   (1.596 ms .. 1.649 ms)
                     0.998 R²   (0.997 R² .. 0.998 R²)
mean                 1.594 ms   (1.572 ms .. 1.618 ms)
std dev              74.22 μs   (61.82 μs .. 95.43 μs)
variance introduced by outliers: 33% (moderately inflated)

benchmarking Day 6/part 1
time                 2.279 ms   (2.209 ms .. 2.358 ms)
                     0.994 R²   (0.990 R² .. 0.997 R²)
mean                 2.351 ms   (2.318 ms .. 2.397 ms)
std dev              133.3 μs   (110.8 μs .. 165.1 μs)
variance introduced by outliers: 40% (moderately inflated)

benchmarking Day 6/part 2
time                 2.907 ms   (2.848 ms .. 2.975 ms)
                     0.995 R²   (0.992 R² .. 0.998 R²)
mean                 2.740 ms   (2.706 ms .. 2.798 ms)
std dev              134.4 μs   (106.4 μs .. 179.3 μs)
variance introduced by outliers: 32% (moderately inflated)

benchmarking Day 7/part 1
time                 15.61 ms   (15.11 ms .. 16.34 ms)
                     0.990 R²   (0.978 R² .. 0.998 R²)
mean                 16.22 ms   (15.95 ms .. 16.49 ms)
std dev              691.7 μs   (515.9 μs .. 954.8 μs)
variance introduced by outliers: 16% (moderately inflated)

benchmarking Day 7/part 2
time                 6.222 ms   (6.172 ms .. 6.282 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 6.301 ms   (6.258 ms .. 6.358 ms)
std dev              151.3 μs   (112.0 μs .. 219.3 μs)

benchmarking Day 8/part 1
time                 1.147 ms   (1.135 ms .. 1.158 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 1.151 ms   (1.144 ms .. 1.160 ms)
std dev              27.40 μs   (22.00 μs .. 37.75 μs)
variance introduced by outliers: 13% (moderately inflated)

benchmarking Day 8/part 2
time                 294.8 ms   (282.0 ms .. 312.1 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 285.8 ms   (283.3 ms .. 290.5 ms)
std dev              4.896 ms   (442.5 μs .. 6.306 ms)
variance introduced by outliers: 16% (moderately inflated)

benchmarking Day 9/part 1
time                 14.61 ms   (13.17 ms .. 15.64 ms)
                     0.978 R²   (0.962 R² .. 0.993 R²)
mean                 14.06 ms   (13.73 ms .. 14.54 ms)
std dev              974.8 μs   (752.3 μs .. 1.246 ms)
variance introduced by outliers: 31% (moderately inflated)

benchmarking Day 9/part 2
time                 18.02 ms   (17.00 ms .. 19.06 ms)
                     0.977 R²   (0.962 R² .. 0.989 R²)
mean                 16.94 ms   (16.46 ms .. 17.47 ms)
std dev              1.374 ms   (1.093 ms .. 1.728 ms)
variance introduced by outliers: 38% (moderately inflated)

benchmarking Day 10/part 1
time                 174.9 μs   (169.0 μs .. 182.4 μs)
                     0.977 R²   (0.961 R² .. 0.989 R²)
mean                 195.4 μs   (184.9 μs .. 208.7 μs)
std dev              39.04 μs   (29.41 μs .. 50.06 μs)
variance introduced by outliers: 95% (severely inflated)

benchmarking Day 10/part 2
time                 190.8 μs   (180.7 μs .. 199.4 μs)
                     0.983 R²   (0.975 R² .. 0.990 R²)
mean                 182.6 μs   (176.1 μs .. 191.7 μs)
std dev              24.75 μs   (17.77 μs .. 37.83 μs)
variance introduced by outliers: 88% (severely inflated)

benchmarking Day 11/part 1
time                 674.1 ms   (597.8 ms .. 733.1 ms)
                     0.999 R²   (0.995 R² .. 1.000 R²)
mean                 642.7 ms   (615.7 ms .. 655.2 ms)
std dev              26.58 ms   (9.120 ms .. 36.42 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking Day 11/part 2
time                 1.263 s    (824.1 ms .. 1.579 s)
                     0.987 R²   (0.953 R² .. 1.000 R²)
mean                 1.339 s    (1.277 s .. 1.376 s)
std dev              62.21 ms   (34.83 ms .. 84.29 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking Day 12/part 1
time                 1.219 ms   (1.174 ms .. 1.279 ms)
                     0.985 R²   (0.979 R² .. 0.992 R²)
mean                 1.354 ms   (1.300 ms .. 1.427 ms)
std dev              230.6 μs   (129.0 μs .. 370.5 μs)
variance introduced by outliers: 89% (severely inflated)

benchmarking Day 12/part 2
time                 1.228 ms   (1.179 ms .. 1.277 ms)
                     0.991 R²   (0.987 R² .. 0.995 R²)
mean                 1.268 ms   (1.240 ms .. 1.313 ms)
std dev              114.4 μs   (87.85 μs .. 165.9 μs)
variance introduced by outliers: 67% (severely inflated)

benchmarking Day 13/part 1
time                 5.643 ms   (5.432 ms .. 5.899 ms)
                     0.985 R²   (0.973 R² .. 0.995 R²)
mean                 6.190 ms   (5.960 ms .. 6.599 ms)
std dev              842.1 μs   (356.3 μs .. 1.299 ms)
variance introduced by outliers: 72% (severely inflated)

benchmarking Day 13/part 2
time                 40.67 μs   (39.17 μs .. 43.14 μs)
                     0.976 R²   (0.960 R² .. 0.989 R²)
mean                 43.13 μs   (41.35 μs .. 47.22 μs)
std dev              9.396 μs   (6.820 μs .. 15.33 μs)
variance introduced by outliers: 96% (severely inflated)

benchmarking Day 14/part 1
time                 5.583 ms   (5.274 ms .. 5.944 ms)
                     0.979 R²   (0.970 R² .. 0.989 R²)
mean                 4.858 ms   (4.678 ms .. 5.054 ms)
std dev              581.4 μs   (498.9 μs .. 695.7 μs)
variance introduced by outliers: 69% (severely inflated)

benchmarking Day 14/part 2
time                 110.9 ms   (108.3 ms .. 114.0 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 109.6 ms   (108.0 ms .. 111.0 ms)
std dev              2.491 ms   (1.919 ms .. 3.091 ms)

benchmarking Day 15/part 1
time                 141.9 μs   (131.9 μs .. 154.8 μs)
                     0.956 R²   (0.933 R² .. 0.987 R²)
mean                 138.7 μs   (131.8 μs .. 148.5 μs)
std dev              25.70 μs   (20.38 μs .. 33.17 μs)
variance introduced by outliers: 94% (severely inflated)

benchmarking Day 15/part 2
time                 2.650 s    (2.335 s .. 2.891 s)
                     0.998 R²   (0.994 R² .. 1.000 R²)
mean                 2.854 s    (2.742 s .. 2.948 s)
std dev              124.2 ms   (98.64 ms .. 142.5 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking Day 16/part 1
time                 41.71 ms   (39.80 ms .. 44.64 ms)
                     0.985 R²   (0.959 R² .. 0.999 R²)
mean                 44.40 ms   (42.94 ms .. 46.00 ms)
std dev              2.896 ms   (2.081 ms .. 3.870 ms)
variance introduced by outliers: 20% (moderately inflated)

benchmarking Day 16/part 2
time                 1.651 s    (1.592 s .. 1.766 s)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 1.657 s    (1.635 s .. 1.677 s)
std dev              23.39 ms   (10.67 ms .. 31.91 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking Day 17/part 1
time                 37.03 ms   (33.46 ms .. 42.02 ms)
                     0.964 R²   (0.934 R² .. 0.997 R²)
mean                 34.13 ms   (33.07 ms .. 36.68 ms)
std dev              3.168 ms   (1.819 ms .. 4.852 ms)
variance introduced by outliers: 36% (moderately inflated)

benchmarking Day 17/part 2
time                 909.5 ms   (856.4 ms .. 1.005 s)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 883.3 ms   (874.5 ms .. 899.1 ms)
std dev              15.10 ms   (2.507 ms .. 19.11 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking Day 18/part 1
time                 11.80 ms   (10.93 ms .. 13.53 ms)
                     0.889 R²   (0.793 R² .. 0.970 R²)
mean                 12.28 ms   (11.57 ms .. 13.22 ms)
std dev              2.121 ms   (1.207 ms .. 3.399 ms)
variance introduced by outliers: 77% (severely inflated)

benchmarking Day 18/part 2
time                 16.36 ms   (13.20 ms .. 18.78 ms)
                     0.904 R²   (0.862 R² .. 0.978 R²)
mean                 12.91 ms   (12.21 ms .. 14.14 ms)
std dev              2.383 ms   (1.302 ms .. 3.447 ms)
variance introduced by outliers: 80% (severely inflated)

benchmarking Day 19/part 1
time                 12.10 ms   (11.27 ms .. 12.78 ms)
                     0.979 R²   (0.962 R² .. 0.992 R²)
mean                 12.94 ms   (12.61 ms .. 13.27 ms)
std dev              952.0 μs   (765.0 μs .. 1.235 ms)
variance introduced by outliers: 36% (moderately inflated)

benchmarking Day 19/part 2
time                 42.93 ms   (35.56 ms .. 49.66 ms)
                     0.951 R²   (0.917 R² .. 0.995 R²)
mean                 42.83 ms   (40.80 ms .. 45.41 ms)
std dev              4.551 ms   (3.700 ms .. 5.572 ms)
variance introduced by outliers: 41% (moderately inflated)

benchmarking Day 20/part 1
time                 46.78 ms   (43.53 ms .. 50.83 ms)
                     0.986 R²   (0.971 R² .. 0.998 R²)
mean                 47.05 ms   (45.71 ms .. 49.61 ms)
std dev              3.605 ms   (1.997 ms .. 5.314 ms)
variance introduced by outliers: 27% (moderately inflated)

benchmarking Day 20/part 2
time                 87.56 ms   (85.12 ms .. 88.84 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 86.89 ms   (86.05 ms .. 87.62 ms)
std dev              1.360 ms   (1.155 ms .. 1.616 ms)

benchmarking Day 21/part 1
time                 15.85 ms   (15.57 ms .. 16.09 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 16.17 ms   (15.98 ms .. 16.86 ms)
std dev              806.5 μs   (229.0 μs .. 1.610 ms)
variance introduced by outliers: 20% (moderately inflated)

benchmarking Day 21/part 2
time                 13.36 ms   (13.02 ms .. 13.69 ms)
                     0.998 R²   (0.997 R² .. 1.000 R²)
mean                 13.22 ms   (13.14 ms .. 13.35 ms)
std dev              250.5 μs   (180.5 μs .. 383.1 μs)

benchmarking Day 22/part 1
time                 172.5 μs   (162.8 μs .. 187.2 μs)
                     0.958 R²   (0.941 R² .. 0.978 R²)
mean                 178.5 μs   (171.7 μs .. 189.6 μs)
std dev              28.42 μs   (22.65 μs .. 35.63 μs)
variance introduced by outliers: 91% (severely inflated)

benchmarking Day 22/part 2
time                 4.690 s    (NaN s .. 4.796 s)
                     1.000 R²   (NaN R² .. 1.000 R²)
mean                 4.877 s    (4.797 s .. 5.026 s)
std dev              146.2 ms   (15.97 ms .. 180.3 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking Day 23/part 1
time                 20.82 μs   (20.06 μs .. 21.66 μs)
                     0.991 R²   (0.985 R² .. 0.997 R²)
mean                 20.03 μs   (19.63 μs .. 20.52 μs)
std dev              1.521 μs   (1.054 μs .. 2.174 μs)
variance introduced by outliers: 76% (severely inflated)

benchmarking Day 23/part 2
time                 1.659 s    (1.247 s .. 1.918 s)
                     0.991 R²   (0.985 R² .. 1.000 R²)
mean                 1.682 s    (1.598 s .. 1.754 s)
std dev              86.86 ms   (71.22 ms .. 99.81 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking Day 24/part 1
time                 3.387 ms   (3.254 ms .. 3.513 ms)
                     0.988 R²   (0.981 R² .. 0.993 R²)
mean                 3.220 ms   (3.139 ms .. 3.293 ms)
std dev              255.6 μs   (230.8 μs .. 288.2 μs)
variance introduced by outliers: 53% (severely inflated)

benchmarking Day 24/part 2
time                 944.6 ms   (934.4 ms .. 958.3 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 949.3 ms   (945.4 ms .. 953.7 ms)
std dev              4.327 ms   (976.7 μs .. 5.993 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking Day 25/part 1
time                 1.440 s    (1.398 s .. 1.465 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.533 s    (1.492 s .. 1.603 s)
std dev              67.31 ms   (6.313 ms .. 84.56 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking Day 25/part 2
time                 2.785 ns   (2.768 ns .. 2.808 ns)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 2.810 ns   (2.790 ns .. 2.830 ns)
std dev              67.67 ps   (53.69 ps .. 94.97 ps)
variance introduced by outliers: 41% (moderately inflated)
```
