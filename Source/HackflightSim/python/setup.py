#!/usr/bin/env python
'''
Python distutils setup for HackflightSim Python loiter

Copyright 2018 Simon D. Levy

MIT License
'''

from distutils.core import setup

setup (name = 'python_loiter',
    version = '0.1',
    install_requires = ['PIL'],
    description = '',
    py_modules = ['python_loiter',],
    author='Simon D. Levy',
    author_email='simon.d.levy@gmail.com',
    license='MIT',
    platforms='Windows'
    )
