from numpy.distutils.core import setup, Extension


ext_modules = [
    Extension(
        name='mknapsack._algos',
        sources=['mknapsack/_algos.f'],
        extra_f77_compile_args=['-std=legacy'],
        f2py_options=['--quiet'],
    )
]


if __name__ == '__main__':
    setup(ext_modules=ext_modules)
