"""Module for solving single 0-1 knapsack problems."""


import logging

from typing import List, Optional

import numpy as np

from mknapsack._algos import mt1, mt2, mt1r
from mknapsack._exceptions import FortranInputCheckError
from mknapsack._utils import preprocess_array, pad_array, check_all_int


logger = logging.getLogger(__name__)


def solve_single_knapsack(
    profits: List[float],
    weights: List[float],
    capacity: float,
    method: str = 'mt2',
    method_kwargs: Optional[dict] = None,
    verbose: bool = False
) -> np.ndarray:
    """Solves the single 0-1 knapsack problem.

    Given a set of items with profits and weights and a knapsack with given
    capacity, which items should we pick in order to maximize profit?

    Args:
        profits: Profit of each item.
        weights: Weight of each item.
        capacity: Capacity of knapsack.
        method:
            Algorithm to use for solving, should be one of

                - 'mt1' - provides a global optimum, but may take a long time
                  to solve for larger problem sizes
                - 'mt2' - provides a fast heuristical solution that might not
                  be the global optimum, but is suitable for larger problems,
                  or an exact solution if required
                - 'mt1r' - variant of 'mt1' but for real numbers

            Defaults to 'mt2'.
        method_kwargs:
            Keyword arguments to pass to a given `method`.

                - 'mt1'
                    * **check_inputs** (int, optional) - Whether to check
                      inputs or not (0=no, 1=yes). Defaults to 1.
                - 'mt2'
                    * **require_exact** (int, optional) - Whether to require an
                      exact solution or not (0=no, 1=yes). Defaults to 0.
                    * **check_inputs** (int, optional) - Whether to check
                      inputs or not (0=no, 1=yes). Defaults to 1.
                - 'mt1r'
                    * **tolerance** (float, optional) - Precision required,
                      two positive values q and r are considered equal if
                      abs(q - r)/max(q, r) .le. tolerance. Defaults to 1e-08.
                    * **check_inputs** (int, optional) - Whether to check
                      inputs or not (0=no, 1=yes). Defaults to 1.

            Defaults to None.

    Returns:
        np.ndarray: Indicator of knapsack assignment for each item, where 0
        means that the item was not assigned to a knapsack.

    Raises:
        FortranInputCheckError: Something is wrong with the inputs when
            validated in the original Fortran source code side.
        ValueError: Something is wrong with the given inputs.

    Example:
        .. code-block:: python

            from mknapsack import solve_single_knapsack

            res = solve_single_knapsack(
                profits=[78, 35, 89, 36, 94, 75, 74, 100, 80, 16],
                weights=[18, 9, 23, 20, 59, 61, 70, 75, 76, 30],
                capacity=190
            )

    References:
        * Silvano Martello, Paolo Toth, Knapsack Problems: Algorithms and
          Computer Implementations, Wiley, 1990, ISBN: 0-471-92420-2,
          LC: QA267.7.M37.

        * `Original Fortran77 source code by Martello and Toth\
          <https://people.sc.fsu.edu/~jburkardt/f77_src/knapsack/knapsack.f>`_
    """
    is_int = (check_all_int(profits) and check_all_int(weights) and
              check_all_int([capacity]))
    real_methods = ['mt1r']

    if is_int:
        profits = preprocess_array(profits)
        weights = preprocess_array(weights)
        if method in real_methods:
            logger.warning('Only integers provided as inputs, but using '
                           f'method="{method}" meant for real numbers')
    else:
        if method not in real_methods:
            raise ValueError(
                f'Floats detected in the inputs, but using method="{method}" '
                'meant for integers')
        profits = preprocess_array(profits, dtype='float32')
        weights = preprocess_array(weights, dtype='float32')

    if len(profits) != len(weights):
        raise ValueError('Profits length must be equal to weights '
                         f'({len(profits) != len(weights)}')

    # Sort items by profit/ratio ratio in ascending order
    items_reorder = (profits / weights).argsort()[::-1]
    items_reorder_reverse = items_reorder.argsort()
    profits = profits[items_reorder]
    weights = weights[items_reorder]

    n = len(profits)

    method = method.lower()
    method_kwargs = method_kwargs or {}
    if method == 'mt1':
        jdim = n + 1
        p = pad_array(profits, jdim)
        w = pad_array(weights, jdim)
        z, x = mt1(
            n=n,
            p=p,
            w=w,
            c=capacity,
            jdim=jdim,
            jck=method_kwargs.pop('check_inputs', 1)
        )

        if z < 0:
            raise FortranInputCheckError(method=method, z=z)

        if verbose:
            logger.info(f'Method: "{method}"')
            logger.info(f'Total profit: {z}')
            logger.info(f'Solution vector (non-original order): {x}')
    elif method == 'mt2':
        jdim = n + 3
        p = pad_array(profits, jdim)
        w = pad_array(weights, jdim)
        z, x, jub = mt2(
            n=n,
            p=p,
            w=w,
            c=capacity,
            jdim=jdim,
            jfo=method_kwargs.pop('require_exact', 0),
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
    elif method == 'mt1r':
        jdim = n + 1
        p = pad_array(profits, jdim)
        w = pad_array(weights, jdim)
        z, x = mt1r(
            n=n,
            p=p,
            w=w,
            c=capacity,
            eps=method_kwargs.pop('tolerance', 1e-08),
            jdim=jdim,
            jck=method_kwargs.pop('check_inputs', 1)
        )

        if z < 0:
            raise FortranInputCheckError(method=method, z=z)

        if verbose:
            logger.info(f'Method: "{method}"')
            logger.info(f'Total profit: {z}')
            logger.info(f'Solution vector (non-original order): {x}')
    else:
        raise ValueError(f'Given method "{method}" not known')

    # Inverse items to original order
    return np.array(x)[:n][items_reorder_reverse]
