#!/usr/bin/env python
'''
Python distutils setup for HackflightSim Python loiter

Copyright 2018 Simon D. Levy

MIT License
'''

from distutils.core import setup

setup (name = 'nengo_picontrol',
    version = '0.1',
    description = 'requires: numpy, nengo',
    py_modules = ['nengo_picontrol',],
    author='Simon D. Levy',
    author_email='simon.d.levy@gmail.com',
    license='MIT',
    platforms='Windows'
    )
