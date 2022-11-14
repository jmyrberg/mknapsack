"""Test cases for multiple knapsack problems."""


import numpy as np
import pytest

from mknapsack._multiple import solve_multiple_knapsack
from mknapsack._exceptions import FortranInputCheckError

from mknapsack.tests.utils import get_id


multiple_knapsack_case_small = {
    'case': 'small',
    'profits': [16, 78, 35, 89, 36, 94, 75, 74, 100, 80],
    'weights': [30, 18, 9, 23, 20, 59, 61, 70, 75, 76],
    'capacities': [100, 90],
    'total_profit': 407,
    'solution': [0, 1, 2, 1, 2, 1, 2, 0, 0, 0]
}

multiple_knapsack_case_medium = {
    'case': 'medium',
    'profits': [78, 35, 89, 36, 94, 75, 74, 100, 80, 16] * 5,
    'weights': [18, 9, 23, 20, 59, 61, 70, 75, 76, 30] * 5,
    'capacities': [90, 100] * 2,
    'total_profit': 1213,
    'solution': [1, 0, 2, 0, 0, 0, 0, 0, 0, 0, 2, 4, 3, 3, 4, 0, 0, 0, 0, 0, 2,
                 3, 4, 1, 0, 0, 0, 0, 0, 0, 3, 4, 2, 3, 0, 0, 0, 0, 0, 0, 2, 1,
                 1, 1, 0, 0, 0, 0, 0, 0]
}

multiple_knapsack_case_large = {
    'case': 'large',
    'profits': [78, 35, 89, 36, 94, 75, 74, 100, 80, 16] * 100_000,
    'weights': [18, 9, 23, 20, 59, 61, 70, 75, 76, 30] * 100_000,
    'capacities': [90, 100] * 500,
    'total_profit': None,  # We just want some solution
    'solution': None
}

multiple_knapsack_success_cases = [
    {
        'method': 'mtm',
        **multiple_knapsack_case_small,
    },
    {
        'method': 'mtm',
        **multiple_knapsack_case_small,
        'verbose': True
    },
    {
        'method': 'mtm',
        **multiple_knapsack_case_medium,
    },
    {
        'method': 'mtm',
        'method_kwargs': {'max_backtracks': 20},
        **multiple_knapsack_case_small,
        'tolerance': 0.03
    },
    {
        'method': 'mthm',
        **multiple_knapsack_case_small
    },
    {
        'method': 'mthm',
        'method_kwargs': {'call_stack': 0},
        **multiple_knapsack_case_small,
        'tolerance': 0.15
    },
    {
        'method': 'mthm',
        **multiple_knapsack_case_medium,
        'tolerance': 0.03
    },
    {
        'method': 'mthm',
        **multiple_knapsack_case_large
    }
]

multiple_knapsack_fail_cases_base = [
    {
        'case': 'profit_weight_mismatch',
        'methods': ['mtm', 'mthm'],
        'profits': [1, 2, 3, 4, 5],
        'weights': [1, 2, 3, 4],
        'capacities': [2, 7],
        'fail_type': ValueError
    },
    {
        'case': 'only_one_item',
        'methods': ['mtm', 'mthm'],
        'profits': [1],
        'weights': [1],
        'capacities': [10],
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'too_many_items',
        'methods': ['mtm'],
        'profits': [1] * 1001,
        'weights': [1] * 1001,
        'capacities': [10],
        'fail_type': ValueError
    },
    {
        'case': 'no_knapsacks',
        'methods': ['mtm', 'mthm'],
        'profits': [1, 2, 3, 4, 5],
        'weights': [1, 2, 3, 4, 5],
        'capacities': [],
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'too_many_knapsacks',
        'methods': ['mtm'],
        'profits': [1, 2, 3, 4, 5],
        'weights': [1, 2, 3, 4, 5],
        'capacities': [4] * 11,
        'fail_type': ValueError
    },
    {
        'case': 'profit_lte_0',
        'methods': ['mtm', 'mthm'],
        'profits': [1, 2, 3, 4, 0],
        'weights': [1, 2, 3, 4, 5],
        'capacities': [2, 7],
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'weight_lte_0',
        'methods': ['mtm', 'mthm'],
        'profits': [1, 2, 3, 4, 5],
        'weights': [1, 2, 3, 4, 0],
        'capacities': [2, 7],
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'capacity_lte_0',
        'methods': ['mtm', 'mthm'],
        'profits': [1, 2, 3, 4, 0],
        'weights': [1, 2, 3, 4, 5],
        'capacities': [2, 0],
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'min_weight_gt_max_capacity',
        'methods': ['mtm', 'mthm'],
        'profits': [1, 2, 3, 4, 5],
        'weights': [8, 9, 10, 11, 12],
        'capacities': [2, 7],
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'max_weight_gt_min_capacity',
        'methods': ['mtm', 'mthm'],
        'profits': [1, 2, 3, 4, 5],
        'weights': [1, 2, 3, 4, 5],
        'capacities': [4],
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'total_weight_le_min_capacity',
        'methods': ['mtm', 'mthm'],
        'profits': [1, 2, 3, 4, 5],
        'weights': [1, 2, 3, 4, 5],
        'capacities': [16],
        'fail_type': FortranInputCheckError
    }
]
multiple_knapsack_fail_cases = [
    {**case, 'method': method}
    for case in multiple_knapsack_fail_cases_base
    for method in case['methods']
]


@pytest.mark.parametrize('params', multiple_knapsack_success_cases, ids=get_id)
def test_solve_multiple_knapsack(params):
    # Get function arguments from params
    profits = params['profits']
    weights = params['weights']
    capacities = params['capacities']

    total_profit = params['total_profit']
    solution = params['solution']
    tolerance = params.get('tolerance', 0)

    func_kwargs = dict(
        profits=profits,
        weights=weights,
        capacities=capacities
    )
    for opt_param in ['method', 'method_kwargs', 'verbose']:
        if opt_param in params:
            func_kwargs[opt_param] = params[opt_param]

    # Run algorithm
    res = solve_multiple_knapsack(**func_kwargs)

    assert isinstance(res, np.ndarray)
    assert len(res) == len(profits)

    # Ensure no overweight in knapsacks
    weights = np.array(weights)
    for i, c in enumerate(capacities):
        assert weights[res == i + 1].sum() <= c

    # Ensure profit within given limits
    res_profit = np.array(profits)[res > 0].sum()
    if total_profit is not None:
        assert res_profit >= (1 - tolerance) * total_profit and \
               res_profit <= (1 + tolerance) * total_profit

    # Ensure global optimum when tolerance = 0
    if solution is not None and tolerance == 0:
        assert np.allclose(res, solution)


@pytest.mark.parametrize('params', multiple_knapsack_fail_cases, ids=get_id)
def test_solve_multiple_knapsack_fail(params):
    del params['case'], params['methods']
    fail_type = params.pop('fail_type')
    with pytest.raises(fail_type):
        solve_multiple_knapsack(**params)
