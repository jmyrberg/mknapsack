"""Module for exceptions."""


class FortranInputCheckError(Exception):
    error_codes = {
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
        }
    }

    def __init__(self, method=None, z=None):
        self.method = method
        self.z = z
        super().__init__()

    def __str__(self):
        if self.method is None or self.z is None:
            return 'Generic exception from Fortran side'
        elif (self.method not in self.error_codes or
              self.z not in self.error_codes[self.method]):
            return ('Generic exception from Fortran side, could not resolve '
                    f'error code {self.z}')
        else:
            return self.error_codes[self.method][self.z]
