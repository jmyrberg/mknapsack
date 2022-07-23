"""Test cases for single knapsack problems."""


import pandas as pd
import pytest

from mknapsack._single import solve_single_knapsack
from mknapsack._exceptions import FortranInputCheckError


single_knapsack_case_small = {
    'case': 'small',
    'profits': [78, 35, 89, 36, 94, 75, 74, 100, 80, 16],
    'weights': [18, 9, 23, 20, 59, 61, 70, 75, 76, 30],
    'capacity': 190,
    'total_profit': 407,
    'solution': [1, 1, 1, 1, 1, 1, 0, 0, 0, 0]
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

single_knapsack_case_large = {
    'case': 'large',
    'profits': [78, 35, 89, 36, 94, 75, 74, 100, 80, 16] * 100_000,
    'weights': [18, 9, 23, 20, 59, 61, 70, 75, 76, 30] * 100_000,
    'capacity': 190 * 500,
    'total_profit': None,  # We just want some solution
    'solution': None
}

single_knapsack_success_cases = [
    {
        'method': 'mt1',
        **single_knapsack_case_small
    },
    {
        'method': 'mt1',
        **single_knapsack_case_small,
        'verbose': True
    },
    {
        'method': 'mt1',
        **single_knapsack_case_medium
    },
    {
        'method': 'mt2',
        **single_knapsack_case_small,
        'tolerance': 1
    },
    {
        'method': 'mt2',
        **single_knapsack_case_medium,
        'tolerance': 0.99
    },
    {
        'method': 'mt2',
        'method_kwargs': {'require_exact': 1},
        **single_knapsack_case_medium,
    },
    {
        'method': 'mt2',
        **single_knapsack_case_large
    }
]

single_knapsack_fail_cases_base = [
    {
        'case': 'profit_weight_mismatch',
        'methods': ['mt1', 'mt2'],
        'profits': [1, 2, 3, 4, 5],
        'weights': [1, 2, 3, 4],
        'capacity': 9,
        'fail_type': ValueError
    },
    {
        'case': 'only_one_item',
        'methods': ['mt1', 'mt2'],
        'profits': [1],
        'weights': [1],
        'capacity': 9,
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'profit_lte_0',
        'methods': ['mt1', 'mt2'],
        'profits': [1, 2, 3, 4, 0],
        'weights': [1, 2, 3, 4, 5],
        'capacity': 9,
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'weight_lte_0',
        'methods': ['mt1', 'mt2'],
        'profits': [1, 2, 3, 4, 5],
        'weights': [1, 2, 3, 4, 0],
        'capacity': 9,
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'capacity_lte_0',
        'methods': ['mt1', 'mt2'],
        'profits': [1, 2, 3, 4, 0],
        'weights': [1, 2, 3, 4, 5],
        'capacity': 0,
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'min_weight_gt_max_capacity',
        'methods': ['mt1', 'mt2'],
        'profits': [1, 2, 3, 4, 5],
        'weights': [8, 9, 10, 11, 12],
        'capacity': 9,
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'total_weight_le_min_capacity',
        'methods': ['mt1', 'mt2'],
        'profits': [1, 2, 3, 4, 5],
        'weights': [1, 2, 3, 4, 5],
        'capacity': 100,
        'fail_type': FortranInputCheckError
    }
]
single_knapsack_fail_cases = [
    {**case, 'method': method}
    for case in single_knapsack_fail_cases_base
    for method in case['methods']
]


def get_id(params):
    method = str(params.get('method', 'NotSet'))
    method_kwargs = str(params.get('method_kwargs', 'NotSet'))
    case = str(params.get('case', 'NotSet'))
    verbose = str(params.get('verbose', 'NotSet'))
    return f'{method}-{method_kwargs}-{verbose}-{case}'


@pytest.mark.parametrize('params', single_knapsack_success_cases, ids=get_id)
def test_solve_single_knapsack(params):
    func_kwargs = dict(
        profits=params['profits'],
        weights=params['weights'],
        capacity=params['capacity']
    )
    for opt_param in ['method', 'method_kwargs', 'verbose']:
        if opt_param in params:
            func_kwargs[opt_param] = params[opt_param]

    res = solve_single_knapsack(**func_kwargs)

    assert isinstance(res, pd.DataFrame)
    assert len(res) == len(params['profits'])

    total_profit = params['total_profit']
    solution = params['solution']
    tolerance = params.get('tolerance', 1)

    # Ensure profit within given limits
    if total_profit is not None:
        min_allowed_profit = tolerance * total_profit
        test_total_profit = res.query('assigned')['profit'].sum()
        if tolerance == 1:
            assert test_total_profit == min_allowed_profit
        else:
            assert test_total_profit >= min_allowed_profit

    # Ensure global optimum when tolerance = 1
    if solution is not None and tolerance == 1:
        test_solution = tuple(res['knapsack_id'].to_list())
        assert tuple(solution) == test_solution


@pytest.mark.parametrize('params', single_knapsack_fail_cases, ids=get_id)
def test_solve_multiple_knapsack_fail(params):
    del params['case'], params['methods']
    fail_type = params.pop('fail_type')
    with pytest.raises(fail_type):
        solve_single_knapsack(**params)
