"""Test cases for generalized assignment problem."""


import numpy as np
import pytest

from mknapsack._generalized_assignment import solve_generalized_assignment
from mknapsack._exceptions import FortranInputCheckError, NoSolutionError, \
    ProblemSizeError

from mknapsack.tests.utils import get_id


generalized_assignment_case_small = {
    'case': 'small',
    'weights': [[4, 1, 2, 1, 4, 3, 8],
                [9, 9, 8, 1, 3, 8, 7]],
    'profits': [[6, 9, 4, 2, 10, 3, 6],
                [4, 8, 9, 1, 7, 5, 4]],
    'capacities': [11, 22],
    'maximize': True,
    'total_profit': 40,
    'solution': [1, 1, 2, 1, 2, 1, 2]
}

generalized_assignment_case_small_reverse = {
    'case': 'small-reverse',
    'weights': np.flip(generalized_assignment_case_small['weights'], 1),
    'profits': np.flip(generalized_assignment_case_small['profits'], 1),
    'capacities': generalized_assignment_case_small['capacities'],
    'maximize': generalized_assignment_case_small['maximize'],
    'total_profit': generalized_assignment_case_small['total_profit'],
    'solution': generalized_assignment_case_small['solution'][::-1]
}

generalized_assignment_case_small2 = {
    'case': 'small-2',
    'profits': [[7, 3, 3, 8, 7],
                [5, 3, 8, 4, 1]],
    'weights': [[8, 2, 8, 9, 1],
                [2, 2, 6, 4, 4]],
    'capacities': [11, 7],
    'maximize': True,
    'total_profit': 22,
    'solution': [2, 1, 1, 2, 1]
}

generalized_assignment_case_medium = {  # yagiura2004/c10400
    'case': 'medium',
    'weights': [
       [12, 15, 25, 13, 15,  6, 14,  9, 22, 24, 16, 18, 10, 14,  9, 23,
        11, 19,  9, 18, 14, 23, 10, 15, 18,  8, 24, 20, 20, 19, 16, 24,
        18, 19,  5, 12, 22, 20, 25, 13, 10, 22, 11, 12, 13, 13, 24, 25,
        10, 21, 16, 17, 10, 20, 23,  6,  6, 25,  7, 17, 24, 17,  5, 15,
        25, 10, 23, 17, 13, 10, 10, 10,  6, 23, 13, 21, 25, 15, 20, 15,
        18, 24, 12, 22, 16,  8,  9, 15,  5, 10,  7, 17,  9,  5, 21,  8,
        18,  8, 19, 10],
       [ 5, 18, 16, 17, 16, 13, 19,  9, 24, 19,  5, 13, 12,  9, 13, 11,
        21,  6, 24, 15,  9,  9, 20,  7, 19,  7, 23, 19,  5, 21, 13, 13,
        21,  6, 14,  8, 21,  9,  6, 15, 13, 25, 18,  8, 25,  6, 24, 19,
        20, 14, 23, 25, 12, 21, 25, 22, 21,  5,  6, 12,  5, 12,  7,  6,
        12, 14,  7, 20, 16, 19, 20, 12, 14, 15, 16,  9, 12, 16, 18, 13,
        22, 23, 14, 16, 18, 14, 22, 17,  5, 14, 18, 12, 10, 11,  7, 20,
         7, 16, 25, 17],
       [ 8, 23, 11, 13, 13, 10, 23, 15, 17, 17, 23, 18, 24, 19, 20, 19,
        23, 10, 11, 15,  5,  6,  8, 13, 16, 10, 10, 25,  9, 16, 13, 10,
        18,  9, 16, 14,  7, 19, 13, 24, 25, 22, 17,  6, 17, 24, 23, 20,
         5, 15, 15,  8, 19, 23, 25,  6, 11, 25, 25,  7, 14, 17,  5, 18,
         6, 12,  6,  8, 12,  8, 15,  6,  5,  9, 14, 24, 15, 11, 15, 21,
        15,  7, 23, 19, 20, 23,  6, 11, 22,  7, 17, 10,  7, 22,  8, 13,
        23,  6, 15,  5],
       [22, 18,  7, 20, 19,  7, 18, 13, 20, 10, 22,  7, 13, 23, 23, 22,
        24, 24, 23,  5, 14, 22, 25,  9, 23, 18, 24,  5, 12, 24, 16,  6,
        22, 14, 14, 11, 21, 17, 13, 10, 22,  6, 10, 12, 10,  6, 13, 14,
        15, 25, 14,  8, 18, 24, 17, 16, 22, 15, 16,  9,  9, 24, 11, 17,
        12, 10,  5,  8, 25,  7, 21,  8, 15, 23, 21, 17, 24,  8,  7, 14,
        25,  6, 12, 22,  5, 22, 24,  5, 25, 16, 18, 16, 23, 24,  7, 20,
        24, 19, 21, 25],
       [21, 24, 20, 23,  6,  6,  8,  9, 21, 22,  6, 11,  5, 10, 16, 17,
         6, 18, 25,  7, 15, 20, 14, 16,  9,  7, 14,  7, 14, 21, 11, 15,
        12,  6, 17, 19,  7, 17, 17, 18, 19, 16, 22, 13,  6, 23, 25, 22,
         5, 12, 12,  9, 11,  5, 24, 15, 18, 16, 22, 12, 23, 12, 16, 11,
        12, 20,  7, 14, 16, 18, 15, 17,  6, 23, 18, 13, 20,  7, 13, 13,
        23, 25, 20, 14, 22, 14, 12, 13, 19, 20, 11, 12, 17, 24, 18, 16,
        14, 14, 18, 22],
       [23, 14,  5, 22, 16,  5, 18, 14, 20, 15, 14, 12, 21, 16,  9, 10,
        23, 25, 14, 21, 17,  5,  5, 25, 23, 21, 10, 14, 21,  7,  5, 25,
        17, 19, 24, 21, 14, 15, 25, 22, 16, 14, 17, 20, 18,  8,  5, 16,
        10, 13, 21, 22, 25,  5, 20, 11, 16, 22, 10, 20, 18, 19, 17,  8,
         8, 11, 16,  9, 17, 17, 13, 14, 15, 15, 21, 14,  6, 10, 25, 25,
        25,  7, 10, 20, 11, 24, 12, 18,  5,  5, 23, 19, 15, 11,  6, 13,
         7, 19, 15, 25],
       [10,  6,  5, 16, 20,  5, 10,  7, 16, 14, 23, 17,  5, 13, 24, 20,
        13, 20, 18,  6,  7, 11, 13, 22, 11, 25,  7, 20,  5, 14, 24, 22,
        23, 14, 11,  9,  7, 21,  7, 19, 17, 19, 13, 16, 12, 13,  8, 18,
        24, 24,  9, 14, 12, 14, 17, 16, 22, 21, 24, 10, 21, 19, 22,  5,
         7, 12, 23, 22, 10, 24, 12, 12, 14, 10, 24, 21, 10, 21, 25,  7,
        17,  5, 17,  8, 10, 23, 13, 15, 11,  5,  5,  5, 12,  7, 24, 16,
        15,  8, 16, 13],
       [16, 19, 14,  8,  7, 12, 19,  5, 10, 13, 11, 15, 18,  7, 22, 19,
        14,  9, 19, 18, 18, 22,  6, 12, 15, 22,  6, 25, 18,  6, 16, 13,
        15,  9, 18,  9, 23, 22, 13,  9, 24, 25, 18, 22, 16,  5, 18, 11,
         5, 15, 15,  9, 17,  5,  9, 10, 22,  8, 23,  6, 20, 12, 10, 11,
         7, 20,  8, 19, 16, 24, 18, 17, 23, 25, 25, 22,  7, 13, 20, 19,
        21, 19, 12, 19, 19, 14, 15, 23,  9, 25, 18, 10, 16, 25, 21, 15,
         7, 16,  6, 19],
       [ 9, 25, 22,  8,  9,  5,  8, 24, 13, 18, 10,  8,  7, 18, 10,  6,
         5, 16, 22, 18, 22, 18, 11,  6, 25, 12, 25,  9,  5,  7, 24, 14,
        25, 23,  7,  5,  5,  5, 14, 24, 12,  9, 21, 16, 19, 21, 13, 15,
         5,  8,  7, 16, 21, 20, 21, 11,  5,  6, 14, 15, 13,  5,  6, 10,
        25, 18, 10, 18, 16, 15,  8, 16, 25,  7, 11, 13, 10, 15, 20, 16,
        21, 24,  6, 22, 24, 18, 18, 24,  5,  7, 15, 10, 16, 15, 22,  6,
        23, 13, 19, 13],
       [19,  9, 20, 20, 25, 18, 22, 20,  8, 11, 22, 16, 25, 22,  7, 25,
         8, 22, 18, 10,  9, 21, 16, 25,  9, 17, 16, 12, 22,  5, 22, 11,
        14, 18, 14, 24, 19,  5, 10,  8, 24, 19, 12,  7, 22, 21, 12, 24,
        10, 11, 10, 11,  7,  8, 25, 14, 10,  7, 10, 13, 13, 22, 16, 11,
        12,  6,  8,  9, 17, 11, 20, 20, 20,  8, 24, 12,  8, 13, 25,  5,
         5, 17, 21,  7, 24, 15, 22,  7,  5, 19,  9, 21,  5, 18,  5, 19,
         6, 17, 15,  8]
    ],
    'profits': [
       [42, 38, 11, 13, 46, 17, 48, 49, 36, 42, 16, 35, 45, 44, 12, 23,
        35, 30, 17, 45, 49, 41, 33, 44, 45, 15, 39, 41, 15, 33, 12, 18,
        43, 16, 22, 42, 18, 19, 24, 17, 17, 24, 29, 21, 37, 43, 39, 28,
        10, 21, 19, 48, 13, 40, 27, 39, 11, 11, 39, 31, 26, 44, 35, 19,
        22, 14, 16, 47, 42, 17, 18, 19, 48, 34, 46, 27, 33, 38, 21, 50,
        26, 41, 49, 16, 29, 26, 33, 46, 37, 30, 37, 47, 12, 32, 21, 38,
        48, 31, 30, 15],
       [42, 14, 18, 29, 22, 39, 16, 44, 10, 42, 30, 33, 13, 18, 37, 40,
        47, 28, 27, 18, 30, 33, 42, 24, 10, 13, 48, 18, 42, 24, 36, 30,
        10, 48, 22, 13, 16, 38, 18, 28, 17, 11, 44, 43, 49, 39, 17, 22,
        49, 20, 38, 47, 43, 49, 47, 36, 44, 30, 17, 34, 18, 25, 44, 24,
        25, 12, 13, 24, 50, 17, 46, 43, 35, 25, 22, 37, 38, 27, 24, 21,
        39, 12, 46, 13, 25, 24, 45, 28, 38, 45, 12, 25, 30, 23, 41, 29,
        36, 26, 40, 15],
       [41, 32, 31, 36, 38, 11, 41, 32, 49, 39, 34, 19, 30, 19, 44, 13,
        32, 25, 24, 40, 30, 13, 32, 31, 24, 31, 18, 24, 34, 45, 15, 45,
        19, 27, 26, 17, 39, 39, 28, 49, 49, 39, 14, 25, 35, 28, 18, 25,
        20, 43, 43, 43, 20, 38, 32, 29, 19, 38, 15, 37, 14, 15, 44, 48,
        48, 41, 34, 33, 15, 11, 38, 21, 18, 13, 43, 41, 29, 21, 19, 39,
        41, 45, 49, 37, 46, 16, 38, 27, 35, 11, 26, 35, 16, 48, 18, 42,
        36, 24, 34, 44],
       [44, 48, 22, 10, 44, 39, 47, 32, 42, 44, 45, 19, 10, 14, 50, 30,
        23, 49, 20, 43, 20, 19, 21, 42, 30, 35, 34, 46, 33, 43, 44, 36,
        20, 27, 16, 37, 10, 42, 10, 29, 38, 42, 16, 11, 12, 30, 20, 22,
        43, 10, 35, 38, 29, 24, 13, 29, 19, 27, 33, 25, 17, 32, 30, 24,
        24, 44, 28, 24, 32, 22, 26, 35, 39, 17, 40, 15, 43, 49, 29, 33,
        35, 33, 47, 34, 44, 40, 16, 42, 45, 27, 35, 15, 14, 30, 50, 11,
        50, 46, 50, 19],
       [27, 16, 19, 47, 15, 25, 45, 39, 38, 32, 36, 18, 41, 22, 18, 41,
        33, 17, 12, 49, 49, 22, 41, 24, 20, 50, 22, 43, 18, 27, 11, 49,
        34, 16, 47, 32, 15, 14, 31, 36, 10, 20, 22, 23, 25, 11, 11, 49,
        36, 37, 16, 20, 46, 16, 16, 14, 30, 25, 14, 14, 26, 12, 17, 21,
        22, 46, 37, 23, 16, 32, 49, 24, 18, 21, 23, 20, 44, 23, 24, 38,
        45, 46, 10, 40, 46, 22, 38, 25, 47, 43, 41, 41, 44, 37, 33, 29,
        28, 48, 35, 33],
       [29, 28, 30, 44, 44, 12, 21, 41, 13, 37, 32, 15, 23, 38, 38, 31,
        28, 31, 27, 17, 43, 16, 30, 26, 42, 45, 48, 37, 39, 31, 23, 16,
        43, 38, 12, 29, 34, 34, 29, 30, 17, 10, 31, 35, 37, 15, 41, 31,
        49, 34, 43, 15, 29, 28, 32, 24, 35, 26, 25, 22, 50, 20, 10, 13,
        10, 42, 46, 23, 39, 40, 36, 39, 41, 27, 10, 48, 17, 41, 16, 43,
        48, 10, 22, 26, 42, 36, 25, 47, 41, 37, 26, 46, 33, 42, 37, 29,
        46, 21, 24, 45],
       [37, 29, 16, 40, 20, 23, 28, 23, 40, 36, 30, 30, 50, 46, 43, 31,
        15, 21, 14, 14, 28, 28, 41, 24, 49, 25, 46, 41, 23, 49, 39, 45,
        42, 14, 24, 13, 32, 23, 27, 27, 23, 14, 41, 25, 21, 15, 27, 40,
        35, 49, 34, 14, 15, 47, 26, 48, 23, 39, 16, 40, 37, 18, 15, 47,
        27, 10, 30, 37, 12, 26, 26, 49, 49, 19, 37, 30, 16, 23, 27, 41,
        19, 45, 49, 32, 36, 28, 17, 26, 11, 21, 40, 36, 31, 16, 29, 41,
        27, 37, 21, 13],
       [42, 40, 23, 20, 15, 33, 32, 22, 14, 43, 19, 36, 13, 27, 30, 46,
        11, 38, 19, 22, 11, 50, 12, 15, 33, 41, 27, 31, 26, 43, 12, 23,
        48, 38, 14, 15, 45, 37, 27, 35, 44, 11, 16, 21, 50, 45, 50, 23,
        16, 24, 19, 28, 42, 41, 29, 32, 41, 17, 36, 39, 44, 38, 47, 13,
        34, 38, 11, 19, 27, 46, 12, 32, 10, 21, 46, 12, 13, 50, 17, 40,
        29, 28, 29, 13, 18, 25, 19, 23, 20, 16, 50, 21, 16, 20, 48, 37,
        39, 16, 13, 23],
       [45, 41, 26, 46, 38, 22, 46, 12, 31, 39, 24, 46, 13, 14, 25, 25,
        16, 24, 25, 31, 35, 45, 17, 24, 30, 41, 10, 36, 44, 32, 16, 38,
        25, 23, 26, 44, 36, 34, 11, 20, 26, 45, 22, 35, 22, 41, 36, 11,
        19, 17, 32, 11, 39, 14, 27, 15, 40, 34, 42, 23, 47, 27, 20, 21,
        43, 12, 22, 48, 35, 37, 30, 27, 12, 20, 17, 44, 28, 50, 20, 32,
        47, 11, 44, 12, 50, 32, 48, 48, 15, 17, 30, 22, 23, 11, 11, 27,
        17, 20, 21, 13],
       [30, 21, 25, 24, 15, 23, 14, 39, 32, 18, 37, 48, 50, 14, 15, 33,
        20, 22, 11, 48, 24, 40, 34, 38, 14, 36, 33, 24, 21, 25, 13, 17,
        24, 35, 28, 12, 19, 47, 43, 40, 29, 34, 26, 30, 39, 22, 17, 50,
        39, 45, 40, 40, 45, 14, 12, 14, 50, 45, 28, 40, 29, 23, 43, 41,
        49, 33, 36, 41, 20, 30, 13, 21, 24, 20, 21, 24, 20, 50, 28, 45,
        34, 25, 48, 10, 36, 32, 28, 27, 39, 44, 12, 42, 37, 16, 49, 23,
        23, 23, 29, 34]],
    'capacities': [480, 490, 461, 491, 484, 477, 472, 469, 474, 484],
    'maximize': True,
    'total_profit': 4627,
    'solution':
        [ 9,  4,  3,  5,  1,  2,  1,  1,  3,  4,  4, 10,  7,  7,  4,  8,  2,
          4,  2,  5,  1,  8,  2,  1,  7,  5,  2,  4,  9,  7,  4,  5,  8,  2,
          5,  9,  8, 10, 10,  3,  3,  9,  2,  2,  8,  8,  8, 10,  2,  7,  3,
          1,  5,  2,  2,  7, 10, 10,  9,  7,  6,  1,  8,  3, 10,  5,  6,  9,
          2,  8,  5,  7,  7,  1,  1,  6,  5,  8,  4,  1,  6,  5,  1,  5,  9,
          4,  9,  9,  5,  2,  8,  1,  5,  3,  4,  3,  4,  5,  4,  6]
}

generalized_assignment_case_medium_minimize = {
    'case': 'medium-minimize',
    'weights': generalized_assignment_case_medium['weights'],
    'profits': generalized_assignment_case_medium['profits'],
    'capacities': generalized_assignment_case_medium['capacities'],
    'maximize': False,
    'total_profit': 1326,
    'solution':
        [ 5,  2,  1,  4,  5,  3, 10,  9,  2, 10,  1,  6,  4,  4,  1,  3,  8,
          5, 10,  7,  8,  3,  8,  8,  2,  2,  9,  2,  1,  2,  5,  6,  2,  7,
          6, 10,  4,  5,  4,  1,  5,  6,  3,  4,  4,  5,  5,  9,  1,  4,  5,
          9,  1,  9, 10,  5,  1,  1,  5,  5,  3,  5,  6,  6,  6,  7,  8,  8,
          7,  3,  8,  1,  8,  3,  6,  8,  8,  3,  6,  2,  7,  6,  5, 10,  8,
          3,  4,  8,  7,  3,  2,  4,  1,  9,  9,  4,  9,  8,  8,  7]
}


generalized_assignment_case_large = {
    'case': 'large',
    'weights': np.tile(generalized_assignment_case_medium['weights'], (4, 4)),
    'profits': np.tile(generalized_assignment_case_medium['profits'], (4, 4)),
    'capacities': generalized_assignment_case_medium['capacities'] * 4,
    'total_profit': None,
    'solution': None
}

generalized_assignment_success_cases = [
    {'method': 'mtg', **generalized_assignment_case_small},
    {'method': 'mtg', **generalized_assignment_case_small_reverse},
    {'method': 'mtg', **generalized_assignment_case_small2},
    {'method': 'mtg', **generalized_assignment_case_medium},
    {'method': 'mtg', **generalized_assignment_case_medium_minimize},
    {'method': 'mtg', **generalized_assignment_case_medium,
     'method_kwargs': {'require_exact': 1}},
    {'method': 'mtg', **generalized_assignment_case_medium_minimize,
     'method_kwargs': {'require_exact': 1}},
    {'method': 'mtg', **generalized_assignment_case_small,
     'method_kwargs': {'max_backtracks': 1, 'require_exact': 0}},
    {'method': 'mthg', **generalized_assignment_case_small},
    {'method': 'mthg', **generalized_assignment_case_small_reverse,
     'tolerance': 0.03},
    {'method': 'mthg', **generalized_assignment_case_small2},
    {'method': 'mthg', **generalized_assignment_case_medium},
    {'method': 'mthg', **generalized_assignment_case_medium_minimize},
    {'method': 'mthg', **generalized_assignment_case_large}
]

generalized_assignment_fail_cases_base = [
    {
        'case': 'mtg_too_many_items',
        'methods': ['mtg'],
        'weights': np.concatenate([
            generalized_assignment_case_medium['weights'],
            np.array(generalized_assignment_case_medium['weights'])[:, -1:]
        ], axis=1),
        'profits': np.concatenate([
            generalized_assignment_case_medium['profits'],
            np.array(generalized_assignment_case_medium['profits'])[:, -1:]
        ], axis=1),
        'capacities': generalized_assignment_case_medium['capacities'],
        'fail_type': ProblemSizeError
    },
    {
        'case': 'mtg_too_many_knapsacks',
        'methods': ['mtg'],
        'weights': np.concatenate([
            generalized_assignment_case_medium['weights'],
            generalized_assignment_case_medium['weights']
        ], axis=0),
        'profits': np.concatenate([
            generalized_assignment_case_medium['profits'],
            generalized_assignment_case_medium['profits']
        ], axis=0),
        'capacities': generalized_assignment_case_medium['capacities'] * 2,
        'fail_type': ProblemSizeError
    },
    {
        'case': 'mthg_too_many_items',
        'methods': ['mthg'],
        'weights': np.tile(
            generalized_assignment_case_medium['weights'], (1, 6)),
        'profits': np.tile(
            generalized_assignment_case_medium['profits'], (1, 6)),
        'capacities': generalized_assignment_case_medium['capacities'],
        'fail_type': ProblemSizeError
    },
    {
        'case': 'mthg_too_many_knapsacks',
        'methods': ['mthg'],
        'weights': np.tile(
            generalized_assignment_case_medium['weights'], (6, 1)),
        'profits': np.tile(
            generalized_assignment_case_medium['profits'], (6, 1)),
        'capacities': generalized_assignment_case_medium['capacities'] * 6,
        'fail_type': ProblemSizeError
    },
    {
        'case': 'profits_weights_knapsacks_mismatch',
        'methods': ['mtg'],
        'weights': [[4, 1, 2, 1, 4, 3, 0],
                    [9, 9, 8, 1, 3, 8, 7]],
        'profits': [[6, 9, 4, 2, 10, 3, 6],
                    [4, 8, 9, 1, 7, 5, 4]],
        'capacities': [11, 22, 21],
        'fail_type': ValueError
    },
    {
        'case': 'profits_weights_items_mismatch',
        'methods': ['mtg'],
        'weights': [[4, 1, 2, 1, 4, 3, 0],
                    [9, 9, 8, 1, 3, 8, 7]],
        'profits': [[6, 9, 4, 2, 10, 3, 6, 5],
                    [4, 8, 9, 1, 7, 5, 4, 6]],
        'capacities': [11, 22],
        'fail_type': ValueError
    },
    {
        'case': 'only_one_knapsack',
        'methods': ['mtg', 'mthg'],
        'weights':
            np.array(generalized_assignment_case_small['weights'])[:1, :],
        'profits':
            np.array(generalized_assignment_case_small['profits'])[:1, :],
        'capacities': generalized_assignment_case_small['capacities'][:1],
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'only_one_item',
        'methods': ['mtg', 'mthg'],
        'weights':
            np.array(generalized_assignment_case_small['weights'])[:, :1],
        'profits':
            np.array(generalized_assignment_case_small['profits'])[:, :1],
        'capacities': generalized_assignment_case_small['capacities'],
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'weight_lte_0',
        'methods': ['mtg', 'mthg'],
        'weights': [[4, 1, 2, 1, 4, 3, 0],
                    [9, 9, 8, 1, 3, 8, 7]],
        'profits': [[6, 9, 4, 2, 10, 3, 6],
                    [4, 8, 9, 1, 7, 5, 4]],
        'capacities': [11, 22],
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'profit_lte_0',
        'methods': ['mtg', 'mthg'],
        'weights': [[4, 1, 2, 1, 4, 3, 8],
                    [9, 9, 8, 1, 3, 8, 7]],
        'profits': [[6, 9, 4, 2, 10, 0, 6],
                    [4, 8, 9, 1, 7, 5, 4]],
        'capacities': [11, 22],
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'capacity_lte_0',
        'methods': ['mtg', 'mthg'],
        'weights': [[4, 1, 2, 1, 4, 3, 8],
                    [9, 9, 8, 1, 3, 8, 7]],
        'profits': [[6, 9, 4, 2, 10, 3, 6],
                    [4, 8, 9, 1, 7, 5, 4]],
        'capacities': [11, 0],
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'item_weight_gt_capacity',
        'methods': ['mtg', 'mthg'],
        'weights': [[4, 1, 2, 1, 4, 3, 12],
                    [9, 9, 8, 1, 3, 8, 23]],
        'profits': [[6, 9, 4, 2, 10, 3, 6],
                    [4, 8, 9, 1, 7, 5, 4]],
        'capacities': [11, 22],
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'capacity_lt_weights',
        'methods': ['mtg', 'mthg'],
        'weights': [[4, 1, 2, 1, 4, 3, 8],
                    [9, 9, 8, 7, 6, 8, 7]],
        'profits': [[6, 9, 4, 2, 10, 3, 6],
                    [4, 8, 9, 1, 7, 5, 4]],
        'capacities': [11, 5],
        'fail_type': FortranInputCheckError
    },
    {
        'case': 'no_solution',
        'methods': ['mtg', 'mthg'],
        'weights': [[4, 10, 2, 1, 4, 3, 8],
                    [9, 21, 8, 7, 6, 8, 7]],
        'profits': [[6, 9, 4, 2, 10, 3, 6],
                    [4, 8, 9, 1, 7, 5, 4]],
        'capacities': [11, 22],
        'fail_type': NoSolutionError
    }
]

generalized_assignment_fail_cases = [
    {**case, 'method': method}
    for case in generalized_assignment_fail_cases_base
    for method in case['methods']
]


@pytest.mark.parametrize('params', generalized_assignment_success_cases,
                         ids=get_id)
def test_solve_generalized_assignment(params):
    # Get function arguments from params
    weights = params['weights']
    profits = params['profits']
    capacities = params['capacities']

    total_profit = params['total_profit']
    solution = params['solution']
    tolerance = params.get('tolerance', 0)

    func_kwargs = dict(weights=weights, profits=profits, capacities=capacities)
    for opt_param in ['maximize', 'method', 'method_kwargs', 'verbose']:
        if opt_param in params:
            func_kwargs[opt_param] = params[opt_param]

    # Run algorithm
    res = solve_generalized_assignment(**func_kwargs)

    assert isinstance(res, np.ndarray)
    assert len(res) == len(weights[0])

    def choose(a, c):
        return a[c, range(a.shape[1])]

    # Ensure no overweight in knapsacks
    weights = np.array(weights)
    res_weights = choose(weights, res - 1)
    for i, capacity in enumerate(capacities):
        assert res_weights[res == i + 1].sum() <= capacity

    # Ensure profit within given limits
    profits = np.array(profits)
    res_profit = choose(profits, res - 1).sum()
    if total_profit is not None:
        assert res_profit >= (1 - tolerance) * total_profit and \
               res_profit <= (1 + tolerance) * total_profit

    # Ensure global optimum when tolerance = 0
    if solution is not None and tolerance == 0:
        assert np.allclose(res, solution)


@pytest.mark.parametrize('params', generalized_assignment_fail_cases,
                         ids=get_id)
def test_solve_generalized_assignment_fail(params):
    del params['case'], params['methods']
    fail_type = params.pop('fail_type')
    with pytest.raises(fail_type):
        solve_generalized_assignment(**params)
