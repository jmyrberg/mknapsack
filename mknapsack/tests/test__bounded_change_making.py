"""Test cases for bounded change-making problem."""


import numpy as np
import pytest

from mknapsack._bounded_change_making import solve_bounded_change_making
from mknapsack._exceptions import FortranInputCheckError, NoSolutionError

from mknapsack.tests.utils import get_id


bounded_change_making_case_small = {
    'case': 'small',
    'weights': [18, 9, 23, 20, 59, 61, 70, 75, 76, 30],
    'n_items': [1, 2, 3, 2, 1, 1, 1, 2, 1, 2],
    'capacity': 190,
    'total_profit': 3,
    'solution': [0, 0, 0, 0, 1, 1, 1, 0, 0, 0]
}

bounded_change_making_case_small_reverse = {
    'case': 'small-reverse',
    'weights': [18, 9, 23, 20, 59, 61, 70, 75, 76, 30][::-1],
    'n_items': [1, 2, 3, 2, 1, 1, 1, 2, 1, 2][::-1],
    'capacity': 190,
    'total_profit': 3,
    'solution': [0, 0, 0, 0, 1, 1, 1, 0, 0, 0][::-1]
}

bounded_change_making_case_medium = {
    'case': 'medium',
    'weights': [18, 9, 23, 20, 59, 61, 70, 75, 65, 30] * 5,
    'n_items': [1, 2, 3, 2, 1, 1, 1, 2, 1, 2] * 5,
    'capacity': 190 * 2,
    'total_profit': 6,
    'solution': [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
                 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0]
}

bounded_change_making_case_large = {
    'case': 'large',
    'weights': [18, 9, 23, 20, 59, 61, 70, 75, 76, 30] * 100_000,
    'n_items': [1, 2, 3, 2, 1, 1, 1, 2, 1, 2] * 100_000,
    'capacity': 190 * 500,
    'total_profit': 1268,
    'solution': None
}

bounded_change_making_success_cases = [
    {'method': 'mtcb', **bounded_change_making_case_small},
    {'method': 'mtcb', **bounded_change_making_case_small_reverse},
    {'method': 'mtcb', **bounded_change_making_case_medium},
    {'method': 'mtcb', **bounded_change_making_case_medium,
     'method_kwargs': {'max_backtracks': 50, 'require_exact': 0},
     'tolerance': 0.3},
    {'method': 'mtcb', **bounded_change_making_case_large, 'tolerance': 0.05}
]

bounded_change_making_fail_cases_base = [
    {
        'case': 'only_one_item',
        'methods': ['mtcb'],
        'weights': [1],
        'n_items': [1],
        'capacity': 9,
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'weight_lte_0',
        'methods': ['mtcb'],
        'weights': [1, 2, 3, 4, 0],
        'n_items': [1, 2, 2, 1, 1],
        'capacity': 9,
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'n_items_lte_0',
        'methods': ['mtcb'],
        'weights': [1, 2, 3, 4, 5],
        'n_items': [1, 2, 2, 1, 0],
        'capacity': 9,
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'capacity_lte_0',
        'methods': ['mtcb'],
        'weights': [1, 2, 3, 4, 5],
        'n_items': [1, 2, 2, 1, 1],
        'capacity': 0,
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'item_type_weight_gte_capacity',
        'methods': ['mtcb'],
        'weights': [1, 2, 3, 4, 5],
        'n_items': [1, 2, 2, 1, 3],
        'capacity': 9,
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'total_weight_lte_capacity',
        'methods': ['mtcb'],
        'weights': [1, 2, 3, 4, 5],
        'n_items': [1, 1, 1, 1, 1],
        'capacity': 16,
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'no_solution',
        'methods': ['mtcb'],
        'weights': [2, 4, 6],
        'n_items': [1, 1, 1],
        'capacity': 9,
        'fail_type': NoSolutionError
    },
]

bounded_change_making_fail_cases = [
    {**case, 'method': method}
    for case in bounded_change_making_fail_cases_base
    for method in case['methods']
]


@pytest.mark.parametrize('params', bounded_change_making_success_cases,
                         ids=get_id)
def test_solve_bounded_change_making(params):
    # Get function arguments from params
    weights = params['weights']
    n_items = params['n_items']
    capacity = params['capacity']

    total_profit = params['total_profit']
    solution = params['solution']
    tolerance = params.get('tolerance', 0)

    func_kwargs = dict(weights=weights, n_items=n_items, capacity=capacity)
    for opt_param in ['method', 'method_kwargs', 'verbose']:
        if opt_param in params:
            func_kwargs[opt_param] = params[opt_param]

    # Run algorithm
    res = solve_bounded_change_making(**func_kwargs)

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


@pytest.mark.parametrize('params', bounded_change_making_fail_cases,
                         ids=get_id)
def test_solve_bounded_change_making_fail(params):
    del params['case'], params['methods']
    fail_type = params.pop('fail_type')
    with pytest.raises(fail_type):
        solve_bounded_change_making(**params)
