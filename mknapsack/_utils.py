"""Module for helper functions and classes."""


import numpy as np


def preprocess_array(ar, dtype='int32'):
    """Preprocess array for Fortran inputs."""
    return np.array(ar, dtype=dtype, order='F')


def pad_array(ar, width):
    """Pad one or two-dimensional array with zeros."""
    if ar.ndim == 1:
        pad_width = (0, width - ar.shape[0])
    elif ar.ndim == 2:
        pad_width = ((0, width[0] - ar.shape[0]), (0, width[1] - ar.shape[1]))
    return np.pad(ar, pad_width, mode='constant')


def check_all_int(ar):
    """Check if all input values are integers."""
    return all(i == 0 or i % int(i) == 0 for i in ar)
