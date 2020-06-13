from setuptools import Extension, setup


with open('README.md', 'r') as f:
    long_description = f.read()

with open('VERSION', 'r') as f:
    version = f.read().strip()

ext_modules = []
ext_modules += [Extension(
    "mkp._algorithms_cy.mtm_cy",
    sources=["cpp/mtm_c.cpp", "python/mkp/_algorithms_cy/mtm_cy.pyx"],
    include_dirs=['cpp/'],
    language='c++',
    extra_compile_args=["-std=c++1y"]
)]

setup(
    name='mkp',
    version=version,
    license='MIT',
    description='Algorithms for Multiple Knapsack Problem (MKP)',
    long_description=long_description,
    author='Jesse Myrberg',
    author_email='jesse.myrberg@gmail.com',
    url='https://github.com/jmyrberg/mkp',
    keywords=['algorithm', 'multiple', 'knapsack', 'optimization'],
    setup_requires=[
        'cython>=0.26.1'
    ],
    install_requires=[
        'cython>=0.26.1',
        'numpy>=1.13.3',
        'pandas>=0.20.3'
    ],
    packages=['mkp', 'mkp._algorithms_cy'],
    package_dir={
        'mkp': 'python/mkp',
    },
    ext_modules=ext_modules,
    classifiers=[
        'Development Status :: 2 - Pre-Alpha',
        'Programming Language :: Python :: 3',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: MIT License'
    ]
)
