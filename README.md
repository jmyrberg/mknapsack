# mknapsack

[![CICD](https://github.com/jmyrberg/mknapsack/actions/workflows/push.yml/badge.svg)](https://github.com/jmyrberg/mknapsack/actions/workflows/push.yml)
[![Documentation](https://readthedocs.org/projects/mknapsack/badge/?version=latest)](https://mknapsack.readthedocs.io/en/latest/?badge=latest)

![mknapsack cover](https://github.com/jmyrberg/mknapsack/blob/master/docs/cover.png?raw=true)

Solving knapsack problems with Python using algorithms by [Martello and Toth](https://dl.acm.org/doi/book/10.5555/98124):

* Single 0-1 knapsack problem: MT1, MT2
* Multiple 0-1 knapsack problem: MTM, MTHM

Documentation is available [here](https://mknapsack.readthedocs.io).


## Installation

1. Install Fortran compiler, if you don't already have it
   * MacOS / Linux:
    `brew install gcc`
   * Linux / Windows Subsystem for Linux:
     `sudo apt-get install gfortran`
   * Windows (experimental):
     * `conda install -c conda-forge m2w64-toolchain_win-64`, or
     * [Install MSYS2](https://www.msys2.org) and `pacman -S --needed base-devel mingw-w64-x86_64-toolchain`

2. `pip install -U mknapsack`


## Example usage

### Single 0-1 Knapsack Problem

```python
from mknapsack import solve_single_knapsack

# Given ten items with the following profits and weights:
profits = [78, 35, 89, 36, 94, 75, 74, 79, 80, 16]
weights = [18, 9, 23, 20, 59, 61, 70, 75, 76, 30]

# ...and a knapsack with the following capacity:
capacity = 190

# Assign items into the knapsack while maximizing profit
res = solve_single_knapsack(profits, weights, capacity)
```

### Multiple 0-1 Knapsack Problem

```python
from mknapsack import solve_multiple_knapsack

# Given ten items with the following profits and weights:
profits = [78, 35, 89, 36, 94, 75, 74, 79, 80, 16]
weights = [18, 9, 23, 20, 59, 61, 70, 75, 76, 30]

# ...and two knapsacks with the following capacities:
capacities = [90, 100]

# Assign items into knapsacks while maximizing profit
res = solve_multiple_knapsack(profits, weights, capacities)
```

## References

* [Knapsack problems: algorithms and computer implementations](https://dl.acm.org/doi/book/10.5555/98124) by S. Martello and P. Toth, 1990
* [Original Fortran77 source code](http://people.sc.fsu.edu/~jburkardt/f77_src/knapsack/knapsack.f) by S. Martello and P. Toth

---
Jesse Myrberg (jesse.myrberg@gmail.com)