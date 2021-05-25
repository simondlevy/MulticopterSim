#!/usr/bin/env python3
'''
Debugging utility

Copyright (C) 2021 Simon D. Levy

MIT License
'''

from sys import stdout


def debug(msg):
    print(msg)
    stdout.flush()
