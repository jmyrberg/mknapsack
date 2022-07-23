"""Module for helper functions and classes."""


import numpy as np


def preprocess_array(ar):
    """Preprocess array for Fortran inputs."""
    return np.array(ar, dtype='int32', order='F')


def pad_array(ar, width):
    """Pad array with zeros to given length."""
    assert ar.ndim == 1
    new_ar = np.zeros((width, ), dtype='int32', order='F')
    n = len(ar)
    new_ar[:n] = ar
    return new_ar
