# mknapsack

[![Build Status](https://travis-ci.com/jmyrberg/mknapsack.svg?branch=master)](https://travis-ci.com/jmyrberg/mknapsack)

Algorithms for solving the [Multiple 0-1 Knapsack Problem](http://www.or.deis.unibo.it/kp/Chapter6.pdf) (MKP).
Currently, only the [MTM algorithm by S. Martello and P. Toth (1981)](https://www.sciencedirect.com/science/article/pii/0166218X81900056) is implemented, 
which guarantees an exact solution. This repository contains a Python interface to C++ implementation of the algorithm.


## Installation

`pip install mknapsack`


## Example usage

Given ten items with the following profits and weights:

```python
profits = [78, 35, 89, 36, 94, 75, 74, 79, 80, 16]
weights = [18, 9, 23, 20, 59, 61, 70, 75, 76, 30]
```

and two knapsacks with the following capacities:

```python
capacities = [90, 100]
```

How should we assign these items to knapsacks in order to maximize the profit?

```python
from mknapsack.algorithms import mtm

z, x, bt, glopt = mtm(profits, weights, capacities)
print('Total profit: %d' % z)
print('Solution: %s' % x)
print('Number of backtracks performed: %d' % bt)
print('Global optimum: %s' % glopt)
```

## References

* [MTM algorithm by Martello and Toth](http://people.sc.fsu.edu/~jburkardt/f77_src/knapsack/knapsack.f) (Fortran)
* [MTHM and MTHG algorithms by Jeff Hetherly](https://github.com/jhetherly/python_knapsack) (Python)

---
Jesse Myrberg (jesse.myrberg@gmail.com)