"""Module for solving unbounded knapsack problems."""


import logging

from typing import List, Optional

import numpy as np

from mknapsack._algos import mtu1, mtu2
from mknapsack._exceptions import FortranError, FortranInputCheckError
from mknapsack._utils import preprocess_array, pad_array


logger = logging.getLogger(__name__)


def solve_unbounded_knapsack(
    profits: List[float],
    weights: List[float],
    capacity: float,
    method: str = 'mtu2',
    method_kwargs: Optional[dict] = None,
    verbose: bool = False
) -> np.ndarray:
    """Solves the unbounded knapsack problem.

    Given an unlimited number of items for each item types with given profits
    and weights, and a knapsack with given capacity, how many of each item type
    should be picked to maximize profits?

    Args:
        profits: Profit of each item type.
        weights: Weight of each item type.
        capacity: Capacity of knapsack.
        method:
            Algorithm to use for solving, should be one of
                * 'mtu1' - provides an exact solution

                .. WARNING::
                   No input check is performed on 'mtu', so please make sure
                   the given inputs satisfy all the conditions as originally
                   defined in the references

                * 'mtu2' - enhanced version of mtu1 that provides either an
                  exact or approximal solution

            Defaults to 'mtu2'.
        method_kwargs:
            Keyword arguments to pass to a given `method`.

                - 'mtu1' - None
                - 'mtu2'
                    * **require_exact** (int, optional) - Whether to require an
                      exact solution or not (0=no, 1=yes). Defaults to 0.
                    * **check_inputs** (int, optional) - Whether to check
                      inputs or not (0=no, 1=yes). Defaults to 1.

            Defaults to None.

    Returns:
        np.ndarray: Number of items assigned to the knapsack for each item
        type.

    Raises:
        FortranInputCheckError: Something is wrong with the inputs when
            validated in the original Fortran source code side.
        ValueError: Something is wrong with the given inputs.

    Example:
        .. code-block:: python

            from mknapsack import solve_unbounded_knapsack

            res = solve_unbounded_knapsack(
                profits=[16, 72, 35, 89, 36, 94, 75, 74, 100, 80],
                weights=[30, 18, 9, 23, 20, 59, 61, 70, 75, 76],
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

    if len(profits) != len(weights):
        raise ValueError('Profits length must be equal to weights '
                         f'({len(profits) != len(weights)}')

    # Sort items by profit/ratio ratio in ascending order
    items_reorder = (profits / weights).argsort()[::-1]
    items_reorder_reverse = np.argsort(items_reorder)
    profits = profits[items_reorder]
    weights = weights[items_reorder]

    n = len(profits)

    method = method.lower()
    method_kwargs = method_kwargs or {}
    if method == 'mtu1':
        # TODO: Input validations for 'mtu1' (and all others...?)
        jdim = n + 1
        p = pad_array(profits, jdim)
        w = pad_array(weights, jdim)
        try:
            z, x, jub = mtu1(
                n=n,
                p=p,
                w=w,
                c=capacity,
                rn=1,
                jdim=jdim
            )
        except Exception:
            raise FortranError(
                'Something went wrong when running Fortran code, please check '
                'your inputs')

        x = [int(e) for e in x]

        if z < 0:  # No checks available for this method
            raise FortranInputCheckError(method=method, z=z)

        if verbose:
            logger.info(f'Method: "{method}"')
            logger.info(f'Total profit: {z}')
            logger.info(f'Solution vector (non-original order): {x}')
            logger.info(f'Solution upper bound: {jub}')
    elif method == 'mtu2':
        jdim = n + 1
        p = pad_array(profits, jdim)
        w = pad_array(weights, jdim)
        z, x, jub = mtu2(
            n=n,
            p=p,
            w=w,
            c=capacity,
            jdim=jdim,
            jfo=method_kwargs.get('require_exact', False),
            jck=method_kwargs.get('check_inputs', 1)
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
