"""Module for solving bounded knapsack problems."""


import logging

from typing import List, Optional

import numpy as np

from mknapsack._algos import mtb2
from mknapsack._exceptions import FortranInputCheckError
from mknapsack._utils import preprocess_array, pad_array


logger = logging.getLogger(__name__)


def solve_bounded_knapsack(
    profits: List[float],
    weights: List[float],
    n_items: List[float],
    capacity: float,
    method: str = 'mtb2',
    method_kwargs: Optional[dict] = None,
    verbose: bool = False
) -> np.ndarray:
    """Solves the bounded knapsack problem.

    Given a certain number of item types with profits and weights, and a
    knapsack with given capacity, how many of each item type should be picked
    to maximize profits?

    Args:
        profits: Profit of each item type.
        weights: Weight of each item type.
        n_items: Number of items available for each item type.
        capacity: Capacity of knapsack.
        method: Algorithm to use for solving, currently only 'mtb2' is
            supported. Defaults to 'mtb2'.
        method_kwargs:
            Keyword arguments to pass to 'mtb2' algorithm:

                * **require_exact** (int, optional) - Whether to require an
                  exact solution or not (0=no, 1=yes). Defaults to 0.
                * **check_inputs** (int, optional) - Whether to check
                  inputs or not (0=no, 1=yes). Defaults to 1.

            Defaults to None.
        verbose: Log details of the solution. Defaults to False.

    Returns:
        np.ndarray: Number of items assigned to the knapsack for each item
        type.

    Raises:
        FortranInputCheckError: Something is wrong with the inputs when
            validated in the original Fortran source code side.
        ValueError: Something is wrong with the given inputs.

    Example:
        .. code-block:: python

            from mknapsack import solve_bounded_knapsack

            res = solve_bounded_knapsack(
                profits=[78, 35, 89, 36, 94, 75, 74, 100, 80, 16],
                weights=[18, 9, 23, 20, 59, 61, 70, 75, 76, 30],
                n_items=[1, 2, 3, 2, 2, 1, 2, 2, 1, 4],
                capacity=190
            )

    References:
        * Silvano Martello, Paolo Toth, Knapsack Problems: Algorithms and
          Computer Implementations, Wiley, 1990, ISBN: 0-471-92420-2,
          LC: QA267.7.M37.

        * `Original Fortran77 source code by Martello and Toth\
          <https://people.sc.fsu.edu/~jburkardt/f77_src/knapsack/knapsack.f>`_
    """
    profits = preprocess_array(profits)
    weights = preprocess_array(weights)
    n_items = preprocess_array(n_items)

    if len(profits) != len(weights) or len(profits) != len(n_items):
        raise ValueError(
            'Profits length must be equal to weights and n_items '
            f'(not {len(profits) == len(weights) == len(n_items)}')

    # Sort items by profit/weights ratio in ascending order
    items_reorder = (profits / weights).argsort()[::-1]
    items_reorder_reverse = np.argsort(items_reorder)
    profits = profits[items_reorder]
    weights = weights[items_reorder]
    n_items = n_items[items_reorder]

    n = len(profits)

    method = method.lower()
    method_kwargs = method_kwargs or {}
    if method == 'mtb2':
        jdim1 = n + 1
        jdim2 = n + int(np.ceil(np.log2(n_items).sum())) + 3
        p = pad_array(profits, jdim1)
        w = pad_array(weights, jdim1)
        b = pad_array(n_items, jdim1)
        z, x, jub = mtb2(
            n=n,
            p=p,
            w=w,
            b=b,
            c=capacity,
            jdim1=jdim1,
            jdim2=jdim2,
            jfo=method_kwargs.pop('require_exact', False),
            jfs=1,
            jck=method_kwargs.pop('check_inputs', 1)
        )

        if z < 0:
            raise FortranInputCheckError(method=method, z=z)

        if verbose:
            logger.info(f'Method: "{method}"')
            logger.info(f'Total profit: {z}')
            logger.info(f'Solution vector (non-original order): {x}')
            logger.info(f'Solution upper bound: {jub}')
    else:
        raise ValueError(f'Given method "{method}" not known')

    # Inverse items and knapsacks to original order
    return np.array(x)[:n][items_reorder_reverse]
