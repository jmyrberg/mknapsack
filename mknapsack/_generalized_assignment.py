"""Module for solving generalized assignment problem."""


import logging

from typing import List, Optional

import numpy as np

from mknapsack._algos import mtg, mthg
from mknapsack._exceptions import FortranInputCheckError, NoSolutionError, \
    ProblemSizeError
from mknapsack._utils import preprocess_array, pad_array


logger = logging.getLogger(__name__)


def solve_generalized_assignment(
    profits: List[List[int]],
    weights: List[List[int]],
    capacities: List[int],
    maximize: bool = True,
    method: str = 'mthg',
    method_kwargs: Optional[dict] = None,
    verbose: bool = False
) -> np.ndarray:
    """Solves the generalized assignment problem.

    Given a set of items with knapsack-dependent weights and profits, assign
    items exactly to one knapsack while maximizing or minimizing profit.

    Args:
        profits: Profit of item when assigned to a knapsack, where
            knapsacks are in the rows and items in the columns.
        weights: Weight of each item when assigned to a knapsack, where
            knapsacks are in the rows and items in the columns.
        capacities: Capacity of each knapsack, which should match the number of
            rows in `profits` and `weights`.
        maximize: Whether to maximize or minimize profits. Defaults to True.
        method:
            Algorithm to use for solving, should be one of

                - 'mtg' - provides a fast heuristical solution or an exact
                  solution if required, but is not the most suitable for larger
                  problems

                  The problem size is limited as follows:
                    * Number of knapsacks <= 10
                    * Number of items <= 100

                - 'mthg' - provides a fast heuristical solution that might not
                  be the global optimum, but is suitable for larger problems

                  The problem size is limited as follows:
                    * Number of knapsacks <= 50
                    * Number of items <= 500

            Defaults to 'mthg'.
        method_kwargs:
            Keyword arguments to pass to a given `method`.

                - 'mtg'
                    * **require_exact** (int, optional) - Whether to require an
                      exact solution or not (0=no, 1=yes). Defaults to 0.
                    * **max_backtracks** (int, optional) - The maximum number
                      of backtracks to perform when ``require_exact=0``.
                      Defaults to 100000.
                    * **check_inputs** (int, optional) - Whether to check
                      inputs or not (0=no, 1=yes). Defaults to 1.

                - 'mthg'
                    * **check_inputs** (int, optional) - Whether to check
                      inputs or not (0=no, 1=yes). Defaults to 1.

            Defaults to None.
        verbose: Log details of the solution. Defaults to False.

    Returns:
        np.ndarray: Assigned knapsack for each item.

    Raises:
        NoSolutionError: No feasible solution found.
        ProblemSizeError: When problem is too large to be solved with the given
            method.
        FortranInputCheckError: Something is wrong with the inputs when
            validated in the original Fortran source code side.
        ValueError: Something is wrong with the given inputs.

    Example:
        .. code-block:: python

            from mknapsack import solve_generalized_assignment

            res = solve_generalized_assignment(
                profits=[[6, 9, 4, 2, 10, 3, 6],
                         [4, 8, 9, 1, 7, 5, 4]],
                weights=[[4, 1, 2, 1, 4, 3, 8],
                         [9, 9, 8, 1, 3, 8, 7]],
                capacities=[11, 22],
                maximize=True
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

    m_profits = profits.shape[0]
    m_weights = weights.shape[0]
    n_profits = profits.shape[1]
    n_weights = weights.shape[1]
    n = n_profits
    m = len(capacities)

    if m_profits != m or m_weights != m:
        raise ValueError(
            f'Number of rows in profits ({m_profits}) or weights '
            f'({m_weights}) is not equal to the length of capacities')

    if n_profits != n_weights:
        raise ValueError('Profits length must be equal to weights '
                         f'({n_profits != n_weights}')

    # The problem size is limited on Fortran side
    error_msg = (
        'The number of {0} {1} exceeds {2}, and there is no guarantee '
        'that the algorithm will work as intended or provides a solution '
        'at all')

    method = method.lower()
    method_kwargs = method_kwargs or {}
    if method == 'mtg':
        maxm = 10
        maxn = 100
        if m > maxm:
            raise ProblemSizeError(error_msg.format('knapsacks', m, maxm))
        if n > maxn:
            raise ProblemSizeError(error_msg.format('items', n, maxn))

        p = pad_array(profits, (maxm, maxn))
        w = pad_array(weights, (maxm, maxn))
        c = pad_array(capacities, maxm)

        if method_kwargs.get('require_exact', 0):
            back = -1
        else:
            back = method_kwargs.get('max_backtracks', 100_000)

        z, x, jb = mtg(
            n=n,
            m=m,
            p=p,
            w=w,
            c=c,
            minmax=2 if maximize else 1,
            back=back,
            jck=method_kwargs.get('check_inputs', 1)
        )

        if z == 0:
            raise NoSolutionError('No feasible solution found')
        elif z < 0:
            raise FortranInputCheckError(method=method, z=z)

        if verbose:
            logger.info(f'Method: "{method}"')
            logger.info(f'Total profit: {z}')
            logger.info(f'Solution vector: {x}')
            bound_name = 'Upper' if maximize else 'Lower'
            logger.info(f'{bound_name} bound: {jb}')
    elif method == 'mthg':
        maxm = 50
        maxn = 500

        # The problem size is limited on Fortran side
        error_msg = (
            'The number of {0} {1} exceeds {2}, and there is no guarantee '
            'that the algorithm will work as intended or provides a solution '
            'at all')
        if m > maxm:
            raise ProblemSizeError(error_msg.format('knapsacks', m, maxm))
        if n > maxn:
            raise ProblemSizeError(error_msg.format('items', n, maxn))

        p = pad_array(profits, (maxm, maxn))
        w = pad_array(weights, (maxm, maxn))
        c = pad_array(capacities, maxm)

        z, x = mthg(
            n=n,
            m=m,
            p=p,
            w=w,
            c=c,
            minmax=2 if maximize else 1,
            jck=method_kwargs.get('check_inputs', 1)
        )

        if z == 0:
            raise NoSolutionError('No feasible solution found')
        elif z < 0:
            raise FortranInputCheckError(method=method, z=z)

        if verbose:
            logger.info(f'Method: "{method}"')
            logger.info(f'Total profit: {z}')
            logger.info(f'Solution vector: {x}')
    else:
        raise ValueError(f'Given method "{method}" not known')

    return np.array(x)[:n]
