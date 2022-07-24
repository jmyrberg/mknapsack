"""Test cases for single knapsack problems."""


import numpy as np
import pytest

from mknapsack._bounded import solve_bounded_knapsack
from mknapsack._exceptions import FortranInputCheckError

from tests.utils import get_id


bounded_knapsack_case_small = {
    'case': 'small',
    'profits': [16, 78, 35, 89, 36, 94, 75, 74, 100, 80],
    'weights': [30, 18, 9, 23, 20, 59, 61, 70, 75, 76],
    'n_items': [1] * 10,
    'capacity': 190,
    'total_profit': 407,
    'solution': [0, 1, 1, 1, 1, 1, 1, 0, 0, 0]
}

bounded_knapsack_case_small_reverse = {
    'case': 'small-reverse',
    'profits': [16, 78, 35, 89, 36, 94, 75, 74, 100, 80][::-1],
    'weights': [30, 18, 9, 23, 20, 59, 61, 70, 75, 76][::-1],
    'n_items': ([1] * 10)[::-1],
    'capacity': 190,
    'total_profit': 407,
    'solution': [0, 1, 1, 1, 1, 1, 1, 0, 0, 0][::-1]
}

bounded_knapsack_case_medium = {
    'case': 'medium',
    'profits': [16, 78, 35, 89, 36, 94, 75, 74, 100, 80] * 5,
    'weights': [30, 18, 9, 23, 20, 59, 61, 70, 75, 76] * 5,
    'n_items': [1, 2] * 25,
    'capacity': 190 * 2,
    'total_profit': 1543,
    'solution': [0, 2, 0, 2, 0, 0, 0, 0, 0, 0, 0, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0,
                 2, 1, 2, 0, 0, 0, 0, 0, 0, 0, 2, 1, 2, 0, 0, 0, 0, 0, 0, 0, 2,
                 1, 1, 0, 0, 0, 0, 0, 0]
}

bounded_knapsack_case_large = {
    'case': 'large',
    'profits': [16, 78, 35, 89, 36, 94, 75, 74, 100, 80] * 100_000,
    'weights': [30, 18, 9, 23, 20, 59, 61, 70, 75, 76] * 100_000,
    'n_items': [1, 2, 3, 4, 5] * 200_000,
    'capacity': 190 * 500,
    'total_profit': None,
    'solution': None
}

bounded_knapsack_success_cases = [
    {'method': 'mtb2', **bounded_knapsack_case_small, 'tolerance': 0.03},
    {'method': 'mtb2', **bounded_knapsack_case_small_reverse,
     'tolerance': 0.03},
    {'method': 'mtb2', **bounded_knapsack_case_medium, 'tolerance': 0.1},
    {'method': 'mtb2', 'method_kwargs': {'require_exact': 1},
     **bounded_knapsack_case_medium},
    {'method': 'mtb2', **bounded_knapsack_case_large}
]

bounded_knapsack_fail_cases_base = [
    {
        'case': 'profit_weight_mismatch',
        'methods': ['mtb2'],
        'profits': [1, 2, 3, 4, 5],
        'weights': [1, 2, 3, 4],
        'n_items': [1, 2, 2, 1, 1],
        'capacity': 9,
        'fail_type': ValueError
    },
    {
        'case': 'only_one_item',
        'methods': ['mtb2'],
        'profits': [1],
        'weights': [1],
        'n_items': [1],
        'capacity': 9,
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'profit_lte_0',
        'methods': ['mtb2'],
        'profits': [1, 2, 3, 4, 0],
        'weights': [1, 2, 3, 4, 5],
        'n_items': [1, 2, 2, 1, 1],
        'capacity': 9,
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'weight_lte_0',
        'methods': ['mtb2'],
        'profits': [1, 2, 3, 4, 5],
        'weights': [1, 2, 3, 4, 0],
        'n_items': [1, 2, 2, 1, 1],
        'capacity': 9,
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'capacity_lte_0',
        'methods': ['mtb2'],
        'profits': [1, 2, 3, 4, 0],
        'weights': [1, 2, 3, 4, 5],
        'n_items': [1, 2, 2, 1, 1],
        'capacity': 0,
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'item_type_total_weight_gte_capacity',
        'methods': ['mtb2'],
        'profits': [1, 2, 3, 4, 5],
        'weights': [1, 2, 3, 4, 5],
        'n_items': [1, 2, 2, 1, 2],
        'capacity': 9,
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'total_weight_le_min_capacity',
        'methods': ['mtb2'],
        'profits': [1, 2, 3, 4, 5],
        'weights': [1, 2, 3, 4, 5],
        'n_items': [1, 2, 2, 1, 1],
        'capacity': 100,
        'fail_type': FortranInputCheckError
    }
]

bounded_knapsack_fail_cases = [
    {**case, 'method': method}
    for case in bounded_knapsack_fail_cases_base
    for method in case['methods']
]


@pytest.mark.parametrize('params', bounded_knapsack_success_cases,
                         ids=get_id)
def test_solve_bounded_knapsack(params):
    # Get function arguments from params
    profits = params['profits']
    weights = params['weights']
    capacity = params['capacity']
    n_items = params['n_items']

    total_profit = params['total_profit']
    solution = params['solution']
    tolerance = params.get('tolerance', 0)

    func_kwargs = dict(
        profits=profits,
        weights=weights,
        capacity=capacity,
        n_items=n_items
    )
    for opt_param in ['method', 'method_kwargs', 'verbose']:
        if opt_param in params:
            func_kwargs[opt_param] = params[opt_param]

    # Run algorithm
    res = solve_bounded_knapsack(**func_kwargs)

    assert isinstance(res, np.ndarray)
    assert len(res) == len(profits)

    # Ensure no overweight in knapsack
    assert (np.array(weights) * res).sum() <= capacity

    # Ensure profit within given limits
    res_profit = (np.array(profits) * res).sum()
    if total_profit is not None:
        assert res_profit >= (1 - tolerance) * total_profit and \
               res_profit <= (1 + tolerance) * total_profit

    # Ensure global optimum when tolerance = 0
    if solution is not None and tolerance == 0:
        assert np.allclose(res, solution)


@pytest.mark.parametrize('params', bounded_knapsack_fail_cases, ids=get_id)
def test_solve_bounded_knapsack_fail(params):
    del params['case'], params['methods']
    fail_type = params.pop('fail_type')
    with pytest.raises(fail_type):
        solve_bounded_knapsack(**params)
