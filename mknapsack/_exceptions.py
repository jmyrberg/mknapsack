"""Module for exceptions."""


class NoSolutionError(Exception):
    """Error when no solution to a problem exists."""


class ProblemSizeError(Exception):
    """Error when problem size is too large to be solved."""


class FortranError(Exception):
    """Error when running Fortran code."""


class FortranInputCheckError(Exception):
    """Error in Fortran source code input validation."""
    _error_codes = {
        'mtm': {
            -1: 'Number of items/knapsacks is either too small or large',
            -2: 'Profit, weight or capacity is <= 0',
            -3: 'Minimum weight is greater than smallest knapsack',
            -4: 'Maximum weight is greater than largest knapsack',
            -5: 'Total sum of weights is smaller than largest knapsack',
            -6: 'Items should be ordered in descending profit/weight order',
            -7: 'Knapsacks should be ordered in ascending order'
        },
        'mthm': {
            -1: 'Number of items/knapsacks is either too small or large',
            -2: 'Profit, weight or capacity is <= 0',
            -3: 'Minimum weight is greater than smallest knapsack',
            -4: 'Maximum weight is greater than largest knapsack',
            -5: 'Total sum of weights is smaller than largest knapsack',
            -6: 'Items should be ordered in descending profit/weight order',
            -7: 'Knapsacks should be ordered in ascending order'
        },
        'mt1': {
            -1: 'Number of items is less than 2',
            -2: 'Profit, weight or capacity is <= 0',
            -3: 'One or more of weights is greater than knapsack capacity',
            -4: 'Total weight is smaller than knapsack capacity',
            -5: 'Items should be ordered in descending profit/weight order'
        },
        'mt2': {
            -1: 'Number of items is less than 2',
            -2: 'Profit, weight or capacity is <= 0',
            -3: 'One or more of weights is greater than knapsack capacity',
            -4: 'Total weight is smaller than knapsack capacity',
            -5: 'Items should be ordered in descending profit/weight order'
        },
        'mt1r': {
            -1.0: 'Number of items is less than 2',
            -2.0: 'Profit, weight or capacity is <= 0',
            -3.0: 'One or more of weights is greater than knapsack capacity',
            -4.0: 'Total weight is smaller than knapsack capacity',
            -5.0: 'Items should be ordered in descending profit/weight order'
        },
        'mtb2': {
            -1: 'Number of items is less than 2',
            -2: 'Profit, weight, capacity or n_items is <= 0',
            -3: 'Total weight (weight * total_n_items) of one or more item '
                'types is greater than the knapsack capacity',
            -4: 'Total weight of all items is smaller than or equal to '
                'knapsack capacity',
            -5: 'Problem with preprocessing before Fortran code',
            -6: 'Items should be ordered in descending profit/weight order'
        },
        'mtu2': {
            -1: 'Number of items is less than 2',
            -2: 'Profit, weight or capacity is <= 0',
            -3: 'One or more item weights is greater than knapsack capacity'
        },
        'mtc2': {
            -1: 'Number of items is less than 2',
            -2: 'Weight or capacity is <= 0',
            -3: 'One or more of weights is greater than knapsack capacity'
        },
        'mtcb': {
            -1: 'Number of items is less than 2',
            -2: 'Weight, number of items or capacity is <= 0',
            -3: 'One or more of weights is greater than knapsack capacity',
            -4: 'Total weight (weight * total_n_items) of an item type is '
                'greater than knapsack capacity',
            -5: 'Total weight of all items is smaller than or equal to '
                'knapsack capacity'
        },
        'mtg': {
            -1: 'Number of knapsacks is less than 2',
            -2: 'Number of items is less than 2',
            -3: 'Number of knapsacks cannot be larger than number of bits - 2',
            -4: 'Profit, weight, or capacity is <= 0',
            -5: 'One or more of weights is greater than knapsack capacity',
            -6: 'One or more knapsacks cannot fit any items',
            -7: 'Number of branching trees is too small for the problem size'
        }
    }

    def __init__(self, method=None, z=None):
        self.method = method
        self.z = z
        super().__init__()

    def __str__(self):
        if self.method is None or self.z is None:
            return 'Generic exception from Fortran side'
        elif (self.method not in self._error_codes or
              self.z not in self._error_codes[self.method]):
            return ('Generic exception from Fortran side, could not resolve '
                    f'error code {self.z}')
        else:
            return self._error_codes[self.method][self.z]
