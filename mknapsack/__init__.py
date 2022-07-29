"""Solving knapsack problems with Python."""


__all__ = [
    'solve_bounded_knapsack',
    'solve_change_making',
    'solve_multiple_knapsack',
    'solve_single_knapsack',
    'solve_unbounded_knapsack'
]

import os
import sys


# .libs -folder must be added to dll for Windows and Python >=3.8
# https://github.com/numpy/numpy/issues/14923
extra_dll_dir = os.path.join(os.path.dirname(__file__), '.libs')
if sys.platform == 'win32' and os.path.isdir(extra_dll_dir):
    os.add_dll_directory(extra_dll_dir)


from mknapsack._bounded import solve_bounded_knapsack  # noqa: E402
from mknapsack._change_making import solve_change_making  # noqa: E402
from mknapsack._multiple import solve_multiple_knapsack  # noqa: E402
from mknapsack._single import solve_single_knapsack  # noqa: E402
from mknapsack._unbounded import solve_unbounded_knapsack  # noqa: E402
