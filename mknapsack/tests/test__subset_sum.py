"""Test cases for subset sum problem."""


import numpy as np
import pytest

from mknapsack._subset_sum import solve_subset_sum
from mknapsack._exceptions import FortranInputCheckError

from mknapsack.tests.utils import get_id


subset_sum_case_small = {
    'case': 'small',
    'weights': [4, 1, 8, 1, 4, 2],
    'capacity': 10,
    'total_profit': 10,
    'solution': [0, 1, 1, 1, 0, 0]
}

subset_sum_case_small_no_solution = {
    'case': 'small-no-solution',
    'weights': [2, 2, 8, 4, 2, 4],
    'capacity': 9,
    'total_profit': 8,
    'solution': None
}

subset_sum_case_small_reverse = {
    'case': 'small-reverse',
    'weights': [4, 1, 8, 1, 4, 2][::-1],
    'capacity': 10,
    'total_profit': 10,
    'solution': [0, 1, 1, 1, 0, 0][::-1]
}

subset_sum_case_medium = {
    'case': 'medium',
    'weights': [18, 9, 23, 20, 59, 61, 70, 75, 65, 30] * 10,
    'capacity': 1234,
    'total_profit': 1234,
    'solution': [
       0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1,
       0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
       0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
       0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
}

subset_sum_case_large = {
    'case': 'large',
    'weights': [18, 9, 23, 20, 59, 61, 70, 75, 65, 30] * 1_000_000,
    'capacity': 123456789,
    'total_profit': 123456789,
    'solution': None
}

subset_sum_success_cases = [
    {'method': 'mtsl', **subset_sum_case_small},
    {'method': 'mtsl', **subset_sum_case_small_reverse},
    {'method': 'mtsl', **subset_sum_case_small_no_solution},
    {'method': 'mtsl', **subset_sum_case_small_no_solution,
     'method_kwargs': {'require_exact': 1}},
    {'method': 'mtsl', **subset_sum_case_medium},
    {'method': 'mtsl', **subset_sum_case_large},
    {'method': 'mtsl', **subset_sum_case_large,
     'method_kwargs': {'require_exact': 0, 'max_items': 5},
     'tolerance': 0.05}
]

subset_sum_fail_cases_base = [
    {
        'case': 'method_not_exists',
        'methods': ['mtpl'],
        'weights': [4, 1, 8, 1, 4, 2],
        'capacity': 10,
        'fail_type': ValueError
    },
    {
        'case': 'only_one_item',
        'methods': ['mtsl'],
        'weights': [4],
        'capacity': 10,
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'weight_lte_0',
        'methods': ['mtsl'],
        'weights': [4, 1, 8, 0, 4, 2],
        'capacity': 10,
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'capacity_lte_0',
        'methods': ['mtsl'],
        'weights': [4, 1, 8, 1, 4, 2],
        'capacity': 0,
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'item_weight_gt_capacity',
        'methods': ['mtsl'],
        'weights': [4, 1, 11, 1, 4, 2],
        'capacity': 10,
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'total_weight_lt_capacity',
        'methods': ['mtsl'],
        'weights': [2, 1, 2, 1, 1, 2],
        'capacity': 10,
        'fail_type': FortranInputCheckError
    }
]

subset_sum_fail_cases = [
    {**case, 'method': method}
    for case in subset_sum_fail_cases_base
    for method in case['methods']
]


@pytest.mark.parametrize('params', subset_sum_success_cases, ids=get_id)
def test_solve_subset_sum(params):
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
    res = solve_subset_sum(**func_kwargs)

    assert isinstance(res, np.ndarray)
    assert len(res) == len(weights)

    # Ensure no overweight in knapsack
    weights = np.array(weights)
    res_profit = weights[res == 1].sum()
    assert res_profit <= capacity

    # Ensure profit within given limits
    if total_profit is not None:
        assert res_profit >= (1 - tolerance) * total_profit and \
               res_profit <= (1 + tolerance) * total_profit

    # Ensure global optimum when tolerance = 0
    if solution is not None and tolerance == 0:
        assert np.allclose(res, solution)


@pytest.mark.parametrize('params', subset_sum_fail_cases, ids=get_id)
def test_solve_subset_sum_fail(params):
    del params['case'], params['methods']
    fail_type = params.pop('fail_type')
    with pytest.raises(fail_type):
        solve_subset_sum(**params)
