# mkp

Library for solving the [Multiple Knapsack Problem](http://www.or.deis.unibo.it/kp/Chapter6.pdf) with MTM algorithm, 
as described in [S. Martello and P. Toth (1981)](https://www.sciencedirect.com/science/article/pii/0166218X81900056).
This repository contains a Python interface to a C++ implementation of the algorithm.

## Installation

1. Clone the repository ```git clone https://github.com/jmyrberg/mkp```
2. Run ```pip install -r requirements.txt``` at the root of repository
3. Run ```python setup.py install```

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
from mkp.algorithms import mtm

z,x,bt = mtm(profits, weights, capacities)
print('Total profit: %d' % z)
print('Solution: %s' % x)
print('Number of backtracks performed: %d' % bt)
```

---
Jesse Myrberg (jesse.myrberg@gmail.com)