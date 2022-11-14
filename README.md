# mknapsack

[![CICD](https://github.com/jmyrberg/mknapsack/actions/workflows/push.yml/badge.svg)](https://github.com/jmyrberg/mknapsack/actions/workflows/push.yml)
[![Build](https://github.com/jmyrberg/mknapsack/actions/workflows/wheels.yml/badge.svg)](https://github.com/jmyrberg/mknapsack/actions/workflows/wheels.yml)
[![Documentation](https://readthedocs.org/projects/mknapsack/badge/?version=latest)](https://mknapsack.readthedocs.io/en/latest/?badge=latest)

![mknapsack cover](https://github.com/jmyrberg/mknapsack/blob/master/docs/cover.png?raw=true)

Solving knapsack problems with Python using algorithms by [Martello and Toth](https://dl.acm.org/doi/book/10.5555/98124):

* Single 0-1 knapsack problem: MT1, MT2, MT1R (real numbers)
* Bounded knapsack problem: MTB2
* Unbounded knapsack problem: MTU1, MTU2
* Multiple 0-1 knapsack problem: MTM, MTHM
* Change-making problem: MTC2
* Bounded change-making problem: MTCB
* Generalized assignment problem: MTG, MTHG
* Bin packing problem: MTP
* Subset sum problem: MTSL

Documentation is available [here](https://mknapsack.readthedocs.io).


## Installation

`pip install mknapsack`


## Example usage

### Single 0-1 Knapsack Problem

```python
from mknapsack import solve_single_knapsack

# Given ten items with the following profits and weights:
profits = [78, 35, 89, 36, 94, 75, 74, 79, 80, 16]
weights = [18, 9, 23, 20, 59, 61, 70, 75, 76, 30]

# ...and a knapsack with the following capacity:
capacity = 190

# Assign items into the knapsack while maximizing profits
res = solve_single_knapsack(profits, weights, capacity)
```

If your inputs are real numbers, you may set parameter `method='mt1r'`.

### Bounded Knapsack Problem

```python
from mknapsack import solve_bounded_knapsack

# Given ten item types with the following profits and weights:
profits = [78, 35, 89, 36, 94, 75, 74, 79, 80, 16]
weights = [18, 9, 23, 20, 59, 61, 70, 75, 76, 30]

# ...and the number of items available for each item type:
n_items = [1, 2, 3, 2, 2, 1, 2, 2, 1, 4]

# ...and a knapsack with the following capacity:
capacity = 190

# Assign items into the knapsack while maximizing profits
res = solve_bounded_knapsack(profits, weights, capacity, n_items)
```

### Unbounded Knapsack Problem

```python
from mknapsack import solve_unbounded_knapsack

# Given ten item types with the following profits and weights:
profits = [16, 72, 35, 89, 36, 94, 75, 74, 100, 80]
weights = [30, 18, 9, 23, 20, 59, 61, 70, 75, 76]

# ...and a knapsack with the following capacity:
capacity = 190

# Assign items repeatedly into the knapsack while maximizing profits
res = solve_unbounded_knapsack(profits, weights, capacity, n_items)
```

### Multiple 0-1 Knapsack Problem

```python
from mknapsack import solve_multiple_knapsack

# Given ten items with the following profits and weights:
profits = [78, 35, 89, 36, 94, 75, 74, 79, 80, 16]
weights = [18, 9, 23, 20, 59, 61, 70, 75, 76, 30]

# ...and two knapsacks with the following capacities:
capacities = [90, 100]

# Assign items into the knapsacks while maximizing profits
res = solve_multiple_knapsack(profits, weights, capacities)
```

### Change-Making Problem

```python
from mknapsack import solve_change_making

# Given ten item types with the following weights:
weights = [18, 9, 23, 20, 59, 61, 70, 75, 76, 30]

# ...and a knapsack with the following capacity:
capacity = 190

# Fill the knapsack while minimizing the number of items
res = solve_change_making(weights, capacity)
```

### Bounded Change-Making Problem

```python
from mknapsack import solve_bounded_change_making

# Given ten item types with the following weights:
weights = [18, 9, 23, 20, 59, 61, 70, 75, 76, 30]

# ...and the number of items available for each item type:
n_items = [1, 2, 3, 2, 1, 1, 1, 2, 1, 2]

# ...and a knapsack with the following capacity:
capacity = 190

# Fill the knapsack while minimizing the number of items
res = solve_bounded_change_making(weights, n_items, capacity)
```

### Generalized Assignment Problem

```python
from mknapsack import solve_generalized_assignment

# Given seven item types with the following knapsack dependent profits:
profits = [[6, 9, 4, 2, 10, 3, 6],
           [4, 8, 9, 1, 7, 5, 4]]

# ...and the following knapsack dependent weights:
weights = [[4, 1, 2, 1, 4, 3, 8],
           [9, 9, 8, 1, 3, 8, 7]]

# ...and two knapsacks with the following capacities:
capacities = [11, 22]

# Assign items into the knapsacks while maximizing profits
res = solve_generalized_assignment(profits, weights, capacities)
```

### Bin Packing Problem

```python
from mknapsack import solve_bin_packing

# Given six items with the following weights:
weights = [4, 1, 8, 1, 4, 2]

# ...and bins with the following capacity:
capacity = 10

# Assign items into bins while minimizing the number of bins required
res = solve_bin_packing(weights, capacity)
```

### Subset Sum Problem

```python
from mknapsack import solve_subset_sum

# Given six items with the following weights:
weights = [4, 1, 8, 1, 4, 2]

# ...and a knapsack with the following capacity:
capacity = 10

# Choose items to fill the knapsack to the fullest
res = solve_subset_sum(weights, capacity)
```


## References

* [Knapsack problems: algorithms and computer implementations](https://dl.acm.org/doi/book/10.5555/98124) by S. Martello and P. Toth, 1990
* [Original Fortran77 source code](http://people.sc.fsu.edu/~jburkardt/f77_src/knapsack/knapsack.f) by S. Martello and P. Toth

---
Jesse Myrberg (jesse.myrberg@gmail.com)