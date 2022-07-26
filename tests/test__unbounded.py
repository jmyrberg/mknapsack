"""Test cases for unbounded knapsack problems."""


import numpy as np
import pytest

from mknapsack._unbounded import solve_unbounded_knapsack
from mknapsack._exceptions import FortranInputCheckError

from tests.utils import get_id


unbounded_knapsack_case_small = {
    'case': 'small',
    'profits': [16, 72, 35, 89, 36, 94, 75, 74, 100, 80],
    'weights': [30, 18, 9, 23, 20, 59, 61, 70, 75, 76],
    'capacity': 190,
    'total_profit': 755,
    'solution': [0, 10, 1, 0, 0, 0, 0, 0, 0, 0]
}

unbounded_knapsack_case_small_reverse = {  # Ensure ordering works
    'case': 'small-reverse',
    'profits': [16, 72, 35, 89, 36, 94, 75, 74, 100, 80][::-1],
    'weights': [30, 18, 9, 23, 20, 59, 61, 70, 75, 76][::-1],
    'capacity': 190,
    'total_profit': 755,
    'solution': [0, 10, 1, 0, 0, 0, 0, 0, 0, 0][::-1]
}

unbounded_knapsack_case_medium = {
    'case': 'medium',
    'profits': [16, 72, 35, 89, 36, 94, 75, 74, 100, 80] * 5,
    'weights': [30, 18, 9, 23, 20, 59, 61, 70, 75, 76] * 5,
    'capacity': 190 * 2,
    'total_profit': 1512,
    'solution': [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 21, 0, 0, 0, 0, 0, 0, 0, 0]
}

unbounded_knapsack_case_large = {
    'case': 'large',
    'profits': [72, 35, 89, 36, 94, 75, 74, 100, 80, 16] * 100_000,
    'weights': [18, 9, 23, 20, 59, 61, 70, 75, 76, 30] * 100_000,
    'capacity': 190 * 500,
    'total_profit': 379996,
    'solution': None
}

unbounded_knapsack_success_cases = [
    {'method': 'mtu1', **unbounded_knapsack_case_small},
    {'method': 'mtu1', **unbounded_knapsack_case_medium},
    {'method': 'mtu2', **unbounded_knapsack_case_small},
    {'method': 'mtu2', **unbounded_knapsack_case_small, 'verbose': True},
    {'method': 'mtu2', **unbounded_knapsack_case_small_reverse},
    {'method': 'mtu2', **unbounded_knapsack_case_medium},
    {'method': 'mtu2', **unbounded_knapsack_case_large, 'tolerance': 0.99},
    {'method': 'mtu2', **unbounded_knapsack_case_large,
     'method_kwargs': {'require_exact': 1}}
]

unbounded_knapsack_fail_cases_base = [
    {
        'case': 'profit_weight_mismatch',
        'methods': ['mtu1', 'mtu2'],
        'profits': [1, 2, 3, 4, 5],
        'weights': [1, 2, 3, 4],
        'capacity': 9,
        'fail_type': ValueError
    },
    {
        'case': 'only_one_item',
        'methods': ['mtu2'],
        'profits': [1],
        'weights': [1],
        'capacity': 9,
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'profit_lte_0',
        'methods': ['mtu2'],
        'profits': [1, 2, 3, 4, 0],
        'weights': [1, 2, 3, 4, 5],
        'capacity': 9,
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'weight_lte_0',
        'methods': ['mtu2'],
        'profits': [1, 2, 3, 4, 5],
        'weights': [1, 2, 3, 4, 0],
        'capacity': 9,
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'capacity_lte_0',
        'methods': ['mtu2'],
        'profits': [1, 2, 3, 4, 0],
        'weights': [1, 2, 3, 4, 5],
        'capacity': 0,
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'weight_gt_capacity',
        'methods': ['mtu2'],
        'profits': [1, 2, 3, 4, 5],
        'weights': [1, 2, 3, 4, 10],
        'capacity': 9,
        'fail_type': FortranInputCheckError
    }
    # TODO: Tests for 'mtu1', when implemented on the Python side
]

unbounded_knapsack_fail_cases = [
    {**case, 'method': method}
    for case in unbounded_knapsack_fail_cases_base
    for method in case['methods']
]


@pytest.mark.parametrize('params', unbounded_knapsack_success_cases,
                         ids=get_id)
def test_solve_unbounded_knapsack(params):
    # Get function arguments from params
    profits = params['profits']
    weights = params['weights']
    capacity = params['capacity']

    total_profit = params['total_profit']
    solution = params['solution']
    tolerance = params.get('tolerance', 0)

    func_kwargs = dict(
        profits=profits,
        weights=weights,
        capacity=capacity
    )
    for opt_param in ['method', 'method_kwargs', 'verbose']:
        if opt_param in params:
            func_kwargs[opt_param] = params[opt_param]

    # Run algorithm
    res = solve_unbounded_knapsack(**func_kwargs)

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


@pytest.mark.parametrize('params', unbounded_knapsack_fail_cases, ids=get_id)
def test_solve_unbounded_knapsack_fail(params):
    del params['case'], params['methods']
    fail_type = params.pop('fail_type')
    with pytest.raises(fail_type):
        solve_unbounded_knapsack(**params)
