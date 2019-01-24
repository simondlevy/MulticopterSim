#!/usr/bin/env python
'''
Python distutils setup for MulticopterSim Python loiter

Copyright 2018 Simon D. Levy

MIT License
'''

from distutils.core import setup

setup (name = 'nengo_pidcontrol',
    version = '0.1',
    description = 'requires: numpy, nengo',
    py_modules = ['nengo_pidcontrol',],
    author='Simon D. Levy',
    author_email='simon.d.levy@gmail.com',
    license='MIT',
    platforms='Windows'
    )
