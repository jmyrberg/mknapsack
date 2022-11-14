"""Test cases for bin packing problem."""


import numpy as np
import pytest

from mknapsack._bin_packing import solve_bin_packing
from mknapsack._exceptions import FortranInputCheckError

from mknapsack.tests.utils import get_id


bin_packing_case_small = {
    'case': 'small',
    'weights': [4, 1, 8, 1, 4, 2],
    'capacity': 10,
    'total_profit': 2,
    'solution': [2, 2, 1, 2, 2, 1]
}

bin_packing_case_small_reverse = {
    'case': 'small-reverse',
    'weights': [4, 1, 8, 1, 4, 2][::-1],
    'capacity': 10,
    'total_profit': 2,
    'solution': [2, 2, 1, 2, 2, 1][::-1]
}

bin_packing_case_medium = {
    'case': 'medium',
    'weights': [18, 9, 23, 20, 59, 61, 70, 75, 65, 30] * 10,
    'capacity': 190,
    'total_profit': 23,
    'solution': [
        23, 23, 21, 22, 20, 18,  9,  4, 11,  4, 22,  3, 22,  8, 15, 18,  7,
        5, 13,  5, 23, 23, 21,  7, 12, 17,  6,  2, 13,  3, 23,  5, 21, 22,
        14, 16, 10,  3, 14,  2, 23, 23, 21,  9, 13, 17,  8,  3, 15,  6, 23,
        20, 21, 22, 19, 16,  8,  1, 14, 10, 23,  4, 22, 10, 11, 16,  7,  1,
        15,  8, 23,  2, 21,  6, 20, 17,  6,  4, 11,  7, 22,  1, 21, 22, 19,
        18, 10,  5, 12,  9, 23, 19, 21, 22, 20, 19,  9,  2, 12,  1]
}

bin_packing_case_large = {
    'case': 'large',
    'weights': [18, 9, 23, 20, 59, 61, 70, 75, 65, 30] * 1000,
    'capacity': 190,
    'total_profit': None,
    'solution': None
}

bin_packing_success_cases = [
    {'method': 'mtp', **bin_packing_case_small},
    {'method': 'mtp', **bin_packing_case_small_reverse},
    {'method': 'mtp', **bin_packing_case_medium},
    {'method': 'mtp', **bin_packing_case_large,
     'method_kwargs': {'require_exact': 0, 'max_backtracks': 5000}}
]

bin_packing_fail_cases_base = [
    {
        'case': 'only_one_item',
        'methods': ['mtp'],
        'weights': [4],
        'capacity': 10,
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'weight_lte_0',
        'methods': ['mtp'],
        'weights': [4, 1, 8, 0, 4, 2],
        'capacity': 10,
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'capacity_lte_0',
        'methods': ['mtp'],
        'weights': [4, 1, 8, 1, 4, 2],
        'capacity': 0,
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'item_weight_gt_capacity',
        'methods': ['mtp'],
        'weights': [4, 1, 11, 1, 4, 2],
        'capacity': 10,
        'fail_type': FortranInputCheckError
    }
]

bin_packing_fail_cases = [
    {**case, 'method': method}
    for case in bin_packing_fail_cases_base
    for method in case['methods']
]


@pytest.mark.parametrize('params', bin_packing_success_cases, ids=get_id)
def test_solve_bin_packing(params):
    # Get function arguments from params
    weights = params['weights']
    capacity = params['capacity']

    total_profit = params['total_profit']
    solution = params['solution']
    tolerance = params.get('tolerance', 0)

    func_kwargs = dict(weights=weights, capacity=capacity)
    for opt_param in ['method', 'method_kwargs', 'verbose']:
        if opt_param in params:
            func_kwargs[opt_param] = params[opt_param]

    # Run algorithm
    res = solve_bin_packing(**func_kwargs)

    assert isinstance(res, np.ndarray)
    assert len(res) == len(weights)

    # Ensure no overweight in knapsack
    weights = np.array(weights)
    for i in range(res.max()):
        assert weights[res == i + 1].sum() <= capacity

    # Ensure profit within given limits
    res_profit = res.max()
    if total_profit is not None:
        assert res_profit >= (1 - tolerance) * total_profit and \
               res_profit <= (1 + tolerance) * total_profit

    # Ensure global optimum when tolerance = 0
    if solution is not None and tolerance == 0:
        assert np.allclose(res, solution)


@pytest.mark.parametrize('params', bin_packing_fail_cases, ids=get_id)
def test_solve_bin_packing_fail(params):
    del params['case'], params['methods']
    fail_type = params.pop('fail_type')
    with pytest.raises(fail_type):
        solve_bin_packing(**params)
