"""Module for solving multiple 0-1 knapsack problems."""


import logging
import warnings

from typing import List, Optional

import numpy as np

from mknapsack._algos import mtm, mthm
from mknapsack._exceptions import FortranInputCheckError
from mknapsack._utils import preprocess_array, pad_array


logger = logging.getLogger(__name__)


def solve_multiple_knapsack(
    profits: List[int],
    weights: List[int],
    capacities: List[int],
    method: str = 'mthm',
    method_kwargs: Optional[dict] = None,
    verbose: bool = False
) -> np.ndarray:
    """Solves the multiple 0-1 knapsack problem.

    Given a set of items with profits and weights and knapsacks with given
    capacities, how should assign the items into knapsacks in order to maximize
    profits?

    Args:
        profits: Profit of each item.
        weights: Weight of each item.
        capacities: Capacity of each knapsack.
        method:
            Algorithm to use for solving, should be one of

                - 'mtm' - provides a global optimum, but may take a long time
                  to solve for larger problem sizes
                - 'mthm' - provides a fast heuristical solution that might not
                  be the global optimum, but is suitable for larger problems

            Defaults to 'mthm'.
        method_kwargs:
            Keyword arguments to pass to a given `method`.

                - 'mtm'
                    * **max_backtracks** (int, optional) - Maximum number of
                      backtracks to perform. Setting -1 corresponds to exact
                      solution. Defaults to -1.
                    * **check_inputs** (int, optional) - Whether to check
                      inputs or not (0=no, 1=yes). Defaults to 1.
                - 'mthm'
                    * **call_stack** (int, optional) - Operations to perform on
                      top of the initial solution. Should be one of
                        * 0 = output initial feasible solution
                        * 1 = try to improve solution once
                        * 2 = try to improve solution twice

                      Defaults to 2.
                    * **check_inputs** (int, optional) - Whether to check
                      inputs or not (0=no, 1=yes). Defaults to 1.

            Defaults to None.
        verbose: Log details of the solution. Defaults to False.

    Returns:
        np.ndarray: The corresponding knapsack for each item, where 0 means
        that the item was not assigned to a knapsack.

    Raises:
        FortranInputCheckError: Something is wrong with the inputs when
            validated in the original Fortran source code side.
        ValueError: Something is wrong with the given inputs.

    Example:
        .. code-block:: python

            from mknapsack import solve_multiple_knapsack

            res = solve_multiple_knapsack(
                profits=[78, 35, 89, 36, 94, 75, 74, 100, 80, 16],
                weights=[18, 9, 23, 20, 59, 61, 70, 75, 76, 30],
                capacities=[90, 100]
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
    capacities = preprocess_array(capacities)

    if len(profits) != len(weights):
        raise ValueError('Profits length must be equal to weights '
                         f'({len(profits) != len(weights)}')

    # Sort items by profit/weights ratio in ascending order
    items_reorder = (profits / weights).argsort()[::-1]
    items_reorder_reverse = items_reorder.argsort()
    profits = profits[items_reorder]
    weights = weights[items_reorder]

    # Sort knapsacks by their capacity in ascending order
    knapsacks_reorder = capacities.argsort()
    capacities = capacities[knapsacks_reorder]
    knapsack_reorder_reverse_map = {
        idx + 1: i + 1 for i, idx in enumerate(knapsacks_reorder.argsort())
    }
    knapsack_reorder_reverse_map[0] = 0

    n = len(profits)
    m = len(capacities)

    method = method.lower()
    method_kwargs = method_kwargs or {}
    if method == 'mtm':
        # These are checked Fortran side as well, but would fail at padding
        maxn = 1000
        maxm = 20
        warnm = 10
        if n > maxn:
            raise ValueError(
                f'Number of items ({n}) cannot be greater than {maxn} for '
                f'method="{method}", please try for example method="mthm"')
        if m > maxm:
            raise ValueError(
                f'Number of knapsacks ({m}) cannot be greater than {maxm} for '
                f'method="{method}", please try for example method="mthm"')
        elif verbose and m > warnm:
            warnings.warn(
                    'Using more than 10 knapsacks may cause the problem '
                    'to take too long! Consider using fewer knapsacks.')

        p = pad_array(profits, maxn)
        w = pad_array(weights, maxn)
        c = pad_array(capacities, maxm)
        z, x, back = mtm(
            n=n,
            m=m,
            p=p,
            w=w,
            c=c,
            back=method_kwargs.pop('max_backtracks', -1),
            jck=method_kwargs.pop('check_inputs', 1)
        )

        if z < 0:
            raise FortranInputCheckError(method=method, z=z)

        if verbose:
            logger.info(f'Method: "{method}"')
            logger.info(f'Total profit: {z}')
            logger.info(f'Solution vector (non-original order): {x}')
            logger.info(f'Number of backtracks: {back}')
    elif method == 'mthm':
        p = pad_array(profits, n + 1)
        w = pad_array(weights, n + 1)
        c = pad_array(capacities, m + 1)
        z, x = mthm(
            n=n,
            m=m,
            p=p,
            w=w,
            c=c,
            jdn=n + 1,
            jdm=m + 1,
            li=method_kwargs.pop('call_stack', 2),
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

    # Inverse items and knapsacks to original order
    res = np.array([knapsack_reorder_reverse_map[i]
                    for i in x[:n][items_reorder_reverse]])

    return res
