__all__ = ['solve_multiple_knapsack']

import os
import sys


# .libs -folder must be added to dll for Windows and Python >=3.8
# https://github.com/numpy/numpy/issues/14923
extra_dll_dir = os.path.join(os.path.dirname(__file__), '.libs')
if sys.platform == 'win32' and os.path.isdir(extra_dll_dir):
    os.add_dll_directory(extra_dll_dir)


from mknapsack._multiple import solve_multiple_knapsack  # noqa: E402
