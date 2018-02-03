try:
    from Cython.Build import cythonize,build_ext
except:
    raise ImportError("You must have cython installed in order to use mkp!") 

import os

from distutils.core import setup, Extension


ext_modules = []
ext_modules += cythonize(Extension(
    "mkp._algorithms_cy.mtm_cy", 
    sources=["cpp/mtm_c.cpp","python/mkp/_algorithms_cy/mtm_cy.pyx"], 
    include_dirs=['cpp/'],
    language='c++'))


setup(name='mkp',
      version='1.0',
      description='Algorithms for Multiple Knapsack Problem (MKP)',
      author='Jesse Myrberg',
      author_email='jesse.myrberg@gmail.com',
      url='https://github.com/jmyrberg/mkp',
      license='MIT',
      packages=['mkp','mkp._algorithms_cy'],
      package_dir={
          'mkp': 'python/mkp',
      },
      ext_modules=ext_modules)

