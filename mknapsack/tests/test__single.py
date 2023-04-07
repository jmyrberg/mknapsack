"""Test cases for single knapsack problems."""


import numpy as np
import pytest

from mknapsack._single import solve_single_knapsack
from mknapsack._exceptions import FortranInputCheckError

from mknapsack.tests.utils import get_id


single_knapsack_case_small = {
    'case': 'small',
    'profits': [16, 78, 35, 89, 36, 94, 75, 74, 100, 80],
    'weights': [30, 18, 9, 23, 20, 59, 61, 70, 75, 76],
    'capacity': 190,
    'total_profit': 407,
    'solution': [0, 1, 1, 1, 1, 1, 1, 0, 0, 0]
}

single_knapsack_case_small_real = {
    'case': 'small-real',
    'profits': [16, 78, 35, 89, 36, 94, 75, 74, 100, 80],
    'weights': [30, 18, 0.4, 23, 20, 59, 61, 70, 75, 76],
    'capacity': 190,
    'total_profit': 407,
    'solution': [0, 1, 1, 1, 1, 1, 1, 0, 0, 0]
}

single_knapsack_case_small_reverse = {  # Ensure ordering works
    'case': 'small-reverse',
    'profits': [16, 78, 35, 89, 36, 94, 75, 74, 100, 80][::-1],
    'weights': [30, 18, 9, 23, 20, 59, 61, 70, 75, 76][::-1],
    'capacity': 190,
    'total_profit': 407,
    'solution': [0, 1, 1, 1, 1, 1, 1, 0, 0, 0][::-1]
}

single_knapsack_case_medium = {
    'case': 'medium',
    'profits': [78, 35, 89, 36, 94, 75, 74, 100, 80, 16] * 5,
    'weights': [18, 9, 23, 20, 59, 61, 70, 75, 76, 30] * 5,
    'capacity': 190 * 2,
    'total_profit': 1213,
    'solution': [1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1,
                 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1,
                 1, 1, 0, 0, 0, 0, 0, 0]
}

single_knapsack_case_medium_real = {
    'case': 'medium-real',
    'profits': [78.5, 35.4, 89.9, 36, 94.4, 75.1, 74.9, 99.3, 80.5, 16.1] * 5,
    'weights': [18.5, 8.3, 23.3, 20.2, 59.5, 61.0, 70.0, 75.0, 76.0, 30.0] * 5,
    'capacity': 190 * 2,
    'total_profit': 1221.4,
    'solution': [1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1,
                 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1,
                 1, 1, 0, 0, 0, 0, 0, 0]
}

single_knapsack_case_large = {
    'case': 'large',
    'profits': [78, 35, 89, 36, 94, 75, 74, 100, 80, 16] * 100_000,
    'weights': [18, 9, 23, 20, 59, 61, 70, 75, 76, 30] * 100_000,
    'capacity': 190 * 500,
    'total_profit': None,  # We just want some solution
    'solution': None
}

single_knapsack_success_cases = [
    {'method': 'mt1', **single_knapsack_case_small},
    {'method': 'mt1', **single_knapsack_case_small, 'verbose': True},
    {'method': 'mt1', **single_knapsack_case_small_reverse},
    {'method': 'mt1', **single_knapsack_case_medium},
    {'method': 'mt2', **single_knapsack_case_small, 'tolerance': 0},
    {'method': 'mt2', **single_knapsack_case_medium, 'tolerance': 0.01},
    {'method': 'mt2', 'method_kwargs': {'require_exact': 1},
     **single_knapsack_case_medium},
    {'method': 'mt2', **single_knapsack_case_large},
    {'method': 'mt1r', **single_knapsack_case_small, 'tolerance': 1e-07},
    {'method': 'mt1r', **single_knapsack_case_medium, 'tolerance': 1e-07},
    {'method': 'mt1r', **single_knapsack_case_small_real, 'tolerance': 1e-07},
    {'method': 'mt1r', **single_knapsack_case_medium_real, 'tolerance': 1e-07}
]

single_knapsack_fail_cases_base = [
    {
        'case': 'profit_weight_mismatch',
        'methods': ['mt1', 'mt2', 'mt1r'],
        'profits': [1, 2, 3, 4, 5],
        'weights': [1, 2, 3, 4],
        'capacity': 9,
        'fail_type': ValueError
    },
    {
        'case': 'only_one_item',
        'methods': ['mt1', 'mt2', 'mt1r'],
        'profits': [1],
        'weights': [1],
        'capacity': 9,
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'profit_lte_0',
        'methods': ['mt1', 'mt2', 'mt1r'],
        'profits': [1, 2, 3, 4, 0],
        'weights': [1, 2, 3, 4, 5],
        'capacity': 9,
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'weight_lte_0',
        'methods': ['mt1', 'mt2', 'mt1r'],
        'profits': [1, 2, 3, 4, 5],
        'weights': [1, 2, 3, 4, 0],
        'capacity': 9,
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'capacity_lte_0',
        'methods': ['mt1', 'mt2', 'mt1r'],
        'profits': [1, 2, 3, 4, 0],
        'weights': [1, 2, 3, 4, 5],
        'capacity': 0,
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'min_weight_gt_max_capacity',
        'methods': ['mt1', 'mt2', 'mt1r'],
        'profits': [1, 2, 3, 4, 5],
        'weights': [8, 9, 10, 11, 12],
        'capacity': 9,
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'total_weight_le_min_capacity',
        'methods': ['mt1', 'mt2', 'mt1r'],
        'profits': [1, 2, 3, 4, 5],
        'weights': [1, 2, 3, 4, 5],
        'capacity': 100,
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'float_inputs_with_real_method',
        'methods': ['mt1', 'mt2'],
        'profits': [1.1, 2, 3, 4, 5],
        'weights': [1, 2, 3, 4, 5],
        'capacity': 100,
        'fail_type': ValueError
    }
]

single_knapsack_fail_cases = [
    {**case, 'method': method}
    for case in single_knapsack_fail_cases_base
    for method in case['methods']
]


@pytest.mark.parametrize('params', single_knapsack_success_cases, ids=get_id)
def test_solve_single_knapsack(params):
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
    res = solve_single_knapsack(**func_kwargs)

    assert isinstance(res, np.ndarray)
    assert len(res) == len(profits)

    # Ensure no overweight in knapsack
    assert np.array(weights)[res > 0].sum() <= capacity

    # Ensure profit within given limits
    res_profit = np.array(profits)[res > 0].sum()
    if total_profit is not None:
        assert res_profit >= (1 - tolerance) * total_profit and \
               res_profit <= (1 + tolerance) * total_profit

    # Ensure global optimum when tolerance = 0
    if solution is not None and tolerance == 0:
        assert np.allclose(res, solution)


@pytest.mark.parametrize('params', single_knapsack_fail_cases, ids=get_id)
def test_solve_single_knapsack_fail(params):
    del params['case'], params['methods']
    fail_type = params.pop('fail_type')
    with pytest.raises(fail_type):
        solve_single_knapsack(**params)
