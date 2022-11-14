"""Test cases for change-making problem."""


import numpy as np
import pytest

from mknapsack._change_making import solve_change_making
from mknapsack._exceptions import FortranInputCheckError, NoSolutionError

from mknapsack.tests.utils import get_id


change_making_case_small = {
    'case': 'small',
    'weights': [18, 9, 23, 20, 59, 61, 70, 75, 76, 30],
    'capacity': 190,
    'total_profit': 3,
    'solution': [0, 0, 0, 0, 1, 1, 1, 0, 0, 0]
}

change_making_case_small_reverse = {
    'case': 'small-reverse',
    'weights': [18, 9, 23, 20, 59, 61, 70, 75, 76, 30][::-1],
    'capacity': 190,
    'total_profit': 3,
    'solution': [0, 0, 0, 0, 1, 1, 1, 0, 0, 0][::-1]
}

change_making_case_medium = {
    'case': 'medium',
    'weights': [18, 9, 23, 20, 59, 61, 70, 75, 76, 30] * 5,
    'capacity': 190 * 2,
    'total_profit': 5,
    'solution': [0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0]
}

change_making_case_large = {
    'case': 'large',
    'weights': [18, 9, 23, 20, 59, 61, 70, 75, 76, 30] * 100_000,
    'capacity': 190 * 500,
    'total_profit': 1250,
    'solution': None
}

change_making_success_cases = [
    {'method': 'mtc2', **change_making_case_small},
    {'method': 'mtc2', **change_making_case_small,
     'method_kwargs': {'max_backtracks': 10, 'require_exact': 0},
     'tolerance': 0.4},
    {'method': 'mtc2', **change_making_case_small_reverse},
    {'method': 'mtc2', **change_making_case_medium},
    {'method': 'mtc2', **change_making_case_large}
]

change_making_fail_cases_base = [
    {
        'case': 'only_one_item',
        'methods': ['mtc2'],
        'weights': [1],
        'capacity': 9,
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'weight_lte_0',
        'methods': ['mtc2'],
        'weights': [1, 2, 3, 4, 0],
        'capacity': 9,
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'capacity_lte_0',
        'methods': ['mtc2'],
        'weights': [1, 2, 3, 4, 5],
        'capacity': 0,
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'item_type_weight_gte_capacity',
        'methods': ['mtc2'],
        'weights': [1, 2, 3, 4, 10],
        'capacity': 9,
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'no_solution',
        'methods': ['mtc2'],
        'weights': [2, 4, 6],
        'capacity': 9,
        'fail_type': NoSolutionError
    },
]

change_making_fail_cases = [
    {**case, 'method': method}
    for case in change_making_fail_cases_base
    for method in case['methods']
]


@pytest.mark.parametrize('params', change_making_success_cases,
                         ids=get_id)
def test_solve_change_making(params):
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
    res = solve_change_making(**func_kwargs)

    assert isinstance(res, np.ndarray)
    assert len(res) == len(weights)

    # Ensure no overweight in knapsack
    assert (np.array(weights) * res).sum() == capacity

    # Ensure profit within given limits
    res_profit = res.sum()
    if total_profit is not None:
        assert res_profit >= (1 - tolerance) * total_profit and \
               res_profit <= (1 + tolerance) * total_profit

    # Ensure global optimum when tolerance = 0
    if solution is not None and tolerance == 0:
        assert np.allclose(res, solution)


@pytest.mark.parametrize('params', change_making_fail_cases, ids=get_id)
def test_solve_change_making_fail(params):
    del params['case'], params['methods']
    fail_type = params.pop('fail_type')
    with pytest.raises(fail_type):
        solve_change_making(**params)
