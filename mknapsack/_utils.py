"""Module for helper functions and classes."""


import numpy as np


def preprocess_array(ar, dtype='int32'):
    """Preprocess array for Fortran inputs."""
    return np.array(ar, dtype=dtype, order='F')


def pad_array(ar, width):
    """Pad array with zeros to given length."""
    assert ar.ndim == 1
    new_ar = np.zeros((width, ), dtype=ar.dtype, order='F')
    n = len(ar)
    new_ar[:n] = ar
    return new_ar


def check_all_int(ar):
    """Check if all input values are integers."""
    return all(i == 0 or i % int(i) == 0 for i in ar)
