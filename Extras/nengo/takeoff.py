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

    # Open a log file
    fp = open('nengopid.csv', 'w')

    # Avoid duplicate log entries
    tprev = 0

    with model:

        stdout.write('Hit Play in simulator ... ')
        stdout.flush()

        # Loop until user hits the stop button
        while True:

            # Run the PID controller
            runpid(copter, pid)

            # Get vehicle state from sim
            telem = copter.getState()

            # Extract time from state
            t = telem[0]

            # Extract altitude from state.  Altitude is in NED coordinates,
            # so we negate it to use as input to PID controller.
            z = -telem[9]

            # Write time and altitude to log file
            if t <= 20.0 and t > tprev:
                fp.write('%2.2f,%+3.3f\n' % (t, z))
                fp.flush()
                print(t, z)
                stdout.flush()
                tprev = t

            # Update the simulator
            sim.run(SIM_TIME)

            # Yield to Multicopter thread
            sleep(.001)
