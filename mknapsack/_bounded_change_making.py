"""Module for solving bounded change-making problem."""


import logging

from typing import List, Optional

import numpy as np

from mknapsack._algos import mtcb
from mknapsack._exceptions import FortranInputCheckError, NoSolutionError
from mknapsack._utils import preprocess_array, pad_array


logger = logging.getLogger(__name__)


def solve_bounded_change_making(
    weights: List[int],
    n_items: List[int],
    capacity: int,
    method: str = 'mtcb',
    method_kwargs: Optional[dict] = None,
    verbose: bool = False
) -> np.ndarray:
    """Solves the bounded change-making problem.

    Given a number of items for item types with weights, and a knapsack with
    capacity, find the minimum number of items that add up to the capacity.

    Args:
        weights: Weight of each item type.
        n_items: Number of items available for each item type.
        capacity: Capacity of knapsack.
        method:
            Algorithm to use for solving, should be one of

                - 'mtcb' - provides a fast heuristical solution that might not
                  be the global optimum, but is suitable for larger problems,
                  or an exact solution if required

            Defaults to 'mtcb'.
        method_kwargs:
            Keyword arguments to pass to a given `method`.

                - 'mtcb'
                    * **require_exact** (int, optional) - Whether to require an
                      exact solution or not (0=no, 1=yes). Defaults to 0.
                    * **max_backtracks** (int, optional) - The maximum number
                      of backtracks to perform when ``require_exact=0``.
                      Defaults to 100000.
                    * **check_inputs** (int, optional) - Whether to check
                      inputs or not (0=no, 1=yes). Defaults to 1.

            Defaults to None.
        verbose: Log details of the solution. Defaults to False.

    Returns:
        np.ndarray: Number of items for each item type.

    Raises:
        NoSolutionError: No feasible solution found.
        FortranInputCheckError: Something is wrong with the inputs when
            validated in the original Fortran source code side.
        ValueError: Something is wrong with the given inputs.

    Example:
        .. code-block:: python

            from mknapsack import solve_bounded_change_making

            res = solve_bounded_change_making(
                weights=[18, 9, 23, 20, 59, 61, 70, 75, 76, 30],
                n_items=[1, 2, 3, 2, 1, 1, 1, 2, 3, 2],
                capacity=190
            )

    References:
        * Silvano Martello, Paolo Toth, Knapsack Problems: Algorithms and
          Computer Implementations, Wiley, 1990, ISBN: 0-471-92420-2,
          LC: QA267.7.M37.

        * `Original Fortran77 source code by Martello and Toth\
          <https://people.sc.fsu.edu/~jburkardt/f77_src/knapsack/knapsack.f>`_
    """
    weights = preprocess_array(weights)
    n_items = preprocess_array(n_items)

    n = len(weights)

    if len(weights) != len(n_items):
        raise ValueError('Weights length must be equal to n_items '
                         f'({len(weights) != len(n_items)}')

    method = method.lower()
    method_kwargs = method_kwargs or {}
    if method == 'mtcb':
        jdn = n + 1
        jdl = np.max(weights) - 1
        w = pad_array(weights, jdn)
        b = pad_array(n_items, jdn)
        z, x = mtcb(
            n=n,
            b=b,
            w=w,
            c=capacity,
            jdn=jdn,
            jdl=jdl,
            jfo=method_kwargs.get('require_exact', 0),
            back=method_kwargs.get('max_backtracks', 100_000),
            jck=method_kwargs.get('check_inputs', 1)
        )

        if z == 0:
            raise NoSolutionError('No feasible solution found')
        elif z < 0:
            raise FortranInputCheckError(method=method, z=z)

        if verbose:
            logger.info(f'Method: "{method}"')
            logger.info(f'Total number of items: {z}')
            logger.info(f'Solution vector: {x}')
    else:
        raise ValueError(f'Given method "{method}" not known')

    return np.array(x)[:n]
