#!/usr/bin/env python3
'''
Test simple altitude-hold PID controller

Copyright (C) 2019 Simon D. Levy

MIT License
'''

import nengo
from time import sleep
from sys import stdout
from altitude_hold import buildpid, runpid, startcopter

# Simulator params
SIM_TIME = 0.001

if __name__ == '__main__':

    # initial conditions
    z = 0
    u = 0

    model, pid = buildpid()

    sim = nengo.Simulator(model, progress_bar=False)  

    copter = startcopter()

    with model:

        stdout.write('Hit Play in simulator ... ')
        stdout.flush()

        # Loop until user hits the stop button
        while  True:

            # Run the PID controller
            runpid(copter, pid)

            # Update the simulator
            sim.run(SIM_TIME)

            # Yield to Multicopter thread
            sleep(.001)
