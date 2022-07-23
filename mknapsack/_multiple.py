"""Module for solving multiple 0-1 knapsack problems.

TODO:
    mt1: Single 0-1 knapsack problem
    mt2: Single 0-1 knapsack problem
    skp1: Single 0-1 knapsack problem
    skp2: Single 0-1 knapsack problem

    kp01m: Single 0-1 knapsack problem, items need to be sorted according to
        decreasing profit per unit weight.
    mtb2: Bounded single 0-1 knapsack problem
    mt1r: Single 0-1 knapsack problem with real parameters

    mtu1: Unbounded single knapsack problem
    mtu2: Unbounded single knapsack problem

    mtp: Bin-packing problem
    mts: Small subset-sum problem
    mtsl: Subset-sum problem

    mtc1: Change-making problem
    mtc2: Unbounded change-making problem
    mtcb: Bounded change-making problem

    mtg: Generalized assignment problem
    mthg: Generalized assignment problem with heuristics
"""


import logging

from typing import List, Optional

import numpy as np
import pandas as pd

from mknapsack._algos import mtm, mthm
from mknapsack._exceptions import FortranInputCheckError


logger = logging.getLogger(__name__)


def preprocess_array(ar):
    """Preprocess array for Fortran inputs."""
    return np.array(ar, dtype='int32', order='F')


def pad_array(ar, width):
    """Pad array with zeros to given length."""
    assert ar.ndim == 1
    new_ar = np.zeros((width, ), dtype='int32', order='F')
    n = len(ar)
    new_ar[:n] = ar
    return new_ar


def process_results(profits, weights, capacities, x):
    """Preprocess multiple 0-1 knapsack results."""
    given_knapsacks = pd.DataFrame({
        'knapsack_id': np.arange(len(capacities)) + 1,
        'knapsack_capacity': capacities
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


def solve_multiple_knapsack(
    profits: List[int],
    weights: List[int],
    capacities: List[int],
    method: str = 'mthm',
    method_kwargs: Optional[dict] = None,
    verbose: bool = False
) -> pd.DataFrame:
    """Solves the multiple 0-1 knapsack problem.

    Given a set of items with profits and weights and knapsacks with given
    capacities, how should assign the items into knapsacks in order to maximize
    profits?

    Args:
        profits: Profits of each item.
        weights: Weight of each item.
        capacities: Capacity of each knapsack.
        method:
            Algorithm to use for solving, should be one of

                - 'mtm' - provides a global optimum, but may take a long time
                  to solve for larger problem sizes
                - 'mthm' - provides a fast heuristical solution that might not
                  be the global optimum

            Defaults to 'mthm'.
        method_kwargs:
            Keyword arguments to pass to a given `method`.

                - 'mtm'
                    * **max_backtracks** (int, optional) - Maximum number of
                      backtracks to perform. Setting -1 corresponds to exact
                      solution. Defaults to -1.
                    * **check_inputs** (int) - Whether to check inputs or not
                      (0=no, 1=yes). Defaults to 1.
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

    Returns:
        pd.DataFrame: The corresponding knapsack for each item, where
        ``knapsack_id=0`` means that the item is not assigned to a knapsack.

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
        * Silvano Martello, Paolo Toth, Optimal and canonical solutions of the
          change-making problem, European Journal of Operational Research,
          1980.

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

    # Sort items by profit/ratio ratio in ascending order
    items_order_idx = (profits / weights).argsort()[::-1]
    items_reverse_idx = np.argsort(items_order_idx)
    profits = profits[items_order_idx]
    weights = weights[items_order_idx]

    # Sort knapsacks by their capacity in ascending order
    knapsacks_order_idx = capacities.argsort()
    knapsacks_reverse_idx = knapsacks_order_idx.argsort()
    capacities = capacities[knapsacks_order_idx]

    n = len(profits)
    m = len(capacities)

    method = method.lower()
    method_kwargs = method_kwargs or {}
    if method == 'mtm':
        # These are checked Fortran side as well, but would fail at padding
        maxn = 1000
        maxm = 10
        if n > maxn:
            raise ValueError(
                f'Number of items ({n}) cannot be greater than {maxn} for '
                f'method="{method}", please try for example method="mthm"')
        if m > maxm:
            raise ValueError(
                f'Number of knapsacks ({m}) cannot be greater than {maxm} for '
                f'method="{method}", please try for example method="mthm"')

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
            logger.info('Solution vector: '
                        f'{x[:n][items_reverse_idx].tolist()}')
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
            logger.info('Solution vector: '
                        f'{x[:n][items_reverse_idx].tolist()}')
    else:
        raise ValueError(f'Given method "{method}" not known')

    # Inverse items and knapsacks to original order
    profits = profits[items_reverse_idx]
    weights = weights[items_reverse_idx]
    x = np.array(x)[items_reverse_idx]
    capacities = capacities[knapsacks_reverse_idx]

    res = process_results(profits, weights, capacities, x)

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
