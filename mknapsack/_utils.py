"""Module for helper functions and classes."""


import numpy as np


def preprocess_array(ar, dtype='int32'):
    """Preprocess array for Fortran inputs."""
    return np.array(ar, dtype=dtype, order='F')


def pad_array(ar, width, axis=0):
    """Pad one or two-dimensional array with zeros."""
    assert ar.ndim <= 2
    if axis == 0:
        pad_width = ((0, 0), (0, width - ar.shape[0]))
    else:
        pad_width = ((0, width - ar.shape[1]), (0, 0))
    ar = np.atleast_2d(ar)
    padded = np.pad(ar, pad_width, mode='constant')
    return padded if ar.ndim == 2 else padded.flatten()


def check_all_int(ar):
    """Check if all input values are integers."""
    return all(i == 0 or i % int(i) == 0 for i in ar)
