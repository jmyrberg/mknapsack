"""Solving knapsack problems with Python."""


from . import _version


__version__ = _version.get_versions()['version']

__all__ = [
    'solve_bin_packing',
    'solve_bounded_knapsack',
    'solve_bounded_change_making',
    'solve_change_making',
    'solve_generalized_assignment',
    'solve_multiple_knapsack',
    'solve_single_knapsack',
    'solve_subset_sum',
    'solve_unbounded_knapsack',
    'FortranInputCheckError',
    'NoSolutionError',
    'ProblemSizeError'
]

import os
import sys


# .libs -folder must be added to dll for Windows and Python >=3.8
# https://github.com/numpy/numpy/issues/14923
extra_dll_dir = os.path.join(os.path.dirname(__file__), '.libs')
if sys.platform == 'win32' and os.path.isdir(extra_dll_dir):
    os.add_dll_directory(extra_dll_dir)


from mknapsack._exceptions import FortranInputCheckError, NoSolutionError, \
    ProblemSizeError  # noqa: E402

from mknapsack._bin_packing import solve_bin_packing  # noqa: E402
from mknapsack._bounded import solve_bounded_knapsack  # noqa: E402
from mknapsack._bounded_change_making import solve_bounded_change_making  # noqa: E402, E501
from mknapsack._change_making import solve_change_making  # noqa: E402
from mknapsack._generalized_assignment import solve_generalized_assignment  # noqa: E402, E501
from mknapsack._multiple import solve_multiple_knapsack  # noqa: E402
from mknapsack._single import solve_single_knapsack  # noqa: E402
from mknapsack._subset_sum import solve_subset_sum  # noqa: E402
from mknapsack._unbounded import solve_unbounded_knapsack  # noqa: E402
