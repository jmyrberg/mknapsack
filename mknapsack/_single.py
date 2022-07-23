"""Module for solving multiple 0-1 knapsack problems.

TODO:
    mt1r: Single 0-1 knapsack problem with real parameters

    mtb2: Bounded single 0-1 knapsack problem
    mtu1: Unbounded single knapsack problem
    mtu2: Unbounded single knapsack problem
"""


import logging

from typing import List, Optional

import numpy as np
import pandas as pd

from mknapsack._algos import mt1, mt2
from mknapsack._exceptions import FortranInputCheckError
from mknapsack._utils import preprocess_array, pad_array


logger = logging.getLogger(__name__)


def process_results(profits, weights, capacity, x):
    """Preprocess single 0-1 knapsack results."""
    given_knapsacks = pd.DataFrame({
        'knapsack_id': [1],
        'knapsack_capacity': [capacity]
    })
    no_knapsack = pd.DataFrame([{'knapsack_id': 0, 'knapsack_capacity': 0}])
    knapsacks = pd.concat([no_knapsack, given_knapsacks], axis=0)
    items = (
        pd.DataFrame({
            'item_id': np.arange(len(profits)) + 1,
            'profit': profits,
            'weight': weights,
            'knapsack_id': x[:len(profits)]
        })
        .merge(knapsacks, on='knapsack_id', how='left')
        .assign(assigned=lambda x: x['knapsack_id'] > 0)
    )
    return items


def solve_single_knapsack(
    profits: List[int],
    weights: List[int],
    capacity: int,
    method: str = 'mt2',
    method_kwargs: Optional[dict] = None,
    verbose: bool = False
) -> pd.DataFrame:
    """Solves the single 0-1 knapsack problem.

    Given a set of items with profits and weights and a knapsack with given
    capacity, which items should we pick in order to maximize profit?

    Args:
        profits: Profits of each item.
        weights: Weight of each item.
        capacity: Capacity of knapsack.
        method:
            Algorithm to use for solving, should be one of

                - 'mt1' - provides a global optimum, but may take a long time
                  to solve for larger problem sizes
                - 'mt2' - provides a fast heuristical solution that might not
                  be the global optimum, but is suitable for larger problems,
                  or an exact solution if required

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

            Defaults to None.

    Returns:
        pd.DataFrame: The corresponding knapsack for each item, where
        ``knapsack_id=0`` means that the item is not assigned to a knapsack.

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
    profits = preprocess_array(profits)
    weights = preprocess_array(weights)

    if len(profits) != len(weights):
        raise ValueError('Profits length must be equal to weights '
                         f'({len(profits) != len(weights)}')

    # Sort items by profit/ratio ratio in ascending order
    items_order_idx = (profits / weights).argsort()[::-1]
    items_reverse_idx = np.argsort(items_order_idx)
    profits = profits[items_order_idx]
    weights = weights[items_order_idx]

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
            logger.info('Solution vector: '
                        f'{x[:n][items_reverse_idx].tolist()}')
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
            logger.info('Solution vector: '
                        f'{x[:n][items_reverse_idx].tolist()}')
            logger.info(f'Solution upper bound: {jub}')
    else:
        raise ValueError(f'Given method "{method}" not known')

    # Inverse items and knapsacks to original order
    profits = profits[items_reverse_idx]
    weights = weights[items_reverse_idx]
    x = np.array(x)[items_reverse_idx]

    res = process_results(profits, weights, capacity, x)

    if verbose:
        knapsack_results = (
            res
            .groupby('knapsack_id')
            .agg(
                capacity_used=('weight', 'sum'),
                capacity_available=('knapsack_capacity', 'first'),
                profit=('profit', 'sum'),
                items=('item_id', 'unique')
            )
        )
        logger.info(f'Results by knapsack_id:\n{knapsack_results.to_string()}')

    return res
