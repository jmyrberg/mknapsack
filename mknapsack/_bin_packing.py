"""Module for solving bin packing problem."""


import logging

from typing import List, Optional

import numpy as np

from mknapsack._algos import mtp
from mknapsack._exceptions import FortranInputCheckError
from mknapsack._utils import preprocess_array, pad_array


logger = logging.getLogger(__name__)


def solve_bin_packing(
    weights: List[int],
    capacity: int,
    method: str = 'mtp',
    method_kwargs: Optional[dict] = None,
    verbose: bool = False
) -> np.ndarray:
    """Solves the bin packing problem.

    Given a set of items with weights, assign each item exactly to one bin
    while minimizing the number of bins required.

    Args:
        weights: Weight of each item.
        capacity: Capacity of the bins.
        method:
            Algorithm to use for solving, should be one of

                - 'mtp' - provides a fast heuristical solution or an exact
                  solution if required

            Defaults to 'mtp'.
        method_kwargs:
            Keyword arguments to pass to a given `method`.

                - 'mtp'
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
        np.ndarray: Assigned bin for each item.

    Raises:
        FortranInputCheckError: Something is wrong with the inputs when
            validated in the original Fortran source code side.
        ValueError: Something is wrong with the given inputs.

    Example:
        .. code-block:: python

            from mknapsack import solve_bin_packing

            res = solve_bin_packing(
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

    # Sort items by weight in descending order
    items_reorder = weights.argsort()[::-1]
    items_reorder_reverse = items_reorder.argsort()
    weights = weights[items_reorder]

    method = method.lower()
    method_kwargs = method_kwargs or {}
    if method == 'mtp':
        jdim = n
        w = pad_array(weights, jdim)

        if method_kwargs.get('require_exact', 0):
            back = -1
        else:
            back = method_kwargs.get('max_backtracks', 100_000)

        z, x, lb = mtp(
            n=n,
            w=w,
            c=capacity,
            jdim=jdim,
            back=back,
            jck=method_kwargs.get('check_inputs', 1)
        )

        if z < 0:
            raise FortranInputCheckError(method=method, z=z)

        if verbose:
            logger.info(f'Method: "{method}"')
            logger.info(f'Total profit: {z}')
            logger.info(f'Solution vector: {x}')
            logger.info(f'Lower bound: {lb}')
    else:
        raise ValueError(f'Given method "{method}" not known')

    return np.array(x)[:n][items_reorder_reverse]
