#!/usr/bin/env python3
'''
Test image transmission 

Copyright (C) 2021 Simon D. Levy

MIT License
'''

from time import sleep
from sys import stdout
import numpy as np
from multicopter_sim import Multicopter

def dump(msg):
    print(msg)
    stdout.flush()

if __name__ == '__main__':

    # Create a multicopter simulation
    copter = Multicopter()

    # Start the simulation
    copter.start()

    dump('Hit the start button ... ')

    running = False

    # Loop until user hits the stop button
    while True:

        # Wait until simulator starts up
        if not copter.isReady():
            continue

        if not running:
            dump('Running')
            running = True

        # Quit after simulator quits
        if copter.isDone():
            break

        # Yield to Multicopter thread
        sleep(.001)
