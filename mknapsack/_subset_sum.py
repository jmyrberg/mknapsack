"""Module for solving subset sum problem."""


import logging

from typing import List, Optional

import numpy as np

from mknapsack._algos import mtsl
from mknapsack._exceptions import FortranInputCheckError
from mknapsack._utils import preprocess_array, pad_array


logger = logging.getLogger(__name__)


def solve_subset_sum(
    weights: List[int],
    capacity: int,
    method: str = 'mtsl',
    method_kwargs: Optional[dict] = None,
    verbose: bool = False
) -> np.ndarray:
    """Solves the subset sum problem.

    Given a set of items with weights and a knapsack with capacity, choose
    items to fill the knapsack to the fullest.

    Args:
        weights: Weight of each item.
        capacity: Capacity of the knapsack.
        method:
            Algorithm to use for solving, should be one of

                - 'mtsl' - provides a fast heuristical solution or an exact
                  solution if required

            Defaults to 'mtsl'.
        method_kwargs:
            Keyword arguments to pass to a given `method`.

                - 'mtsl'
                    * **require_exact** (int, optional) - Whether to require an
                      exact solution or not (0=no, 1=yes). Defaults to 0.
                    * **max_items** (int, optional) - The maximum number
                      of items in the core problem when ``require_exact=0``.
                      Defaults to 90.
                    * **check_inputs** (int, optional) - Whether to check
                      inputs or not (0=no, 1=yes). Defaults to 1.

            Defaults to None.
        verbose: Log details of the solution. Defaults to False.

    Returns:
        np.ndarray: Indicator of knapsack assignment for each item, where 0
            means that the item was not assigned to the knapsack.

    Raises:
        FortranInputCheckError: Something is wrong with the inputs when
            validated in the original Fortran source code side.
        ValueError: Something is wrong with the given inputs.

    Example:
        .. code-block:: python

            from mknapsack import solve_subset_sum

            res = solve_subset_sum(
                weights=[4, 1, 8, 1, 4, 2],
                capacity=10
            )

    References:
        * Silvano Martello, Paolo Toth, Knapsack Problems: Algorithms and
          Computer Implementations, Wiley, 1990, ISBN: 0-471-92420-2,
          LC: QA267.7.M37.

        * `Original Fortran77 source code by Martello and Toth\
          <https://people.sc.fsu.edu/~jburkardt/f77_src/knapsack/knapsack.f>`_
    """
    weights = preprocess_array(weights)
    n = len(weights)

    method = method.lower()
    method_kwargs = method_kwargs or {}
    if method == 'mtsl':
        jdn = n + 1
        w = pad_array(weights, jdn)

        jdd = 5000
        if method_kwargs.get('require_exact', 0):
            itmm = jdd
        else:
            itmm = method_kwargs.get('max_items', 90) + 1

        z, x = mtsl(
            n=n,
            w=w,
            c=capacity,
            jdn=jdn,
            jdd=jdd,
            itmm=itmm,
            jck=method_kwargs.get('check_inputs', 1)
        )

        if z < 0:
            raise FortranInputCheckError(method=method, z=z)

        if verbose:
            logger.info(f'Method: "{method}"')
            logger.info(f'Total profit: {z}')
            logger.info(f'Solution vector: {x}')
    else:
        raise ValueError(f'Given method "{method}" not known')

    return np.array(x)[:n]
