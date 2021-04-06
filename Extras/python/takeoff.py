#!/usr/bin/env python3
'''
Test simple altitude-hold PID controller

Copyright (C) 2019 Simon D. Levy

MIT License
'''

from time import sleep
from sys import stdout
import numpy as np
from pidcontroller import AltitudePidController
from multicopter_sim import Multicopter

# Target
ALTITUDE_TARGET = 10

# PID params
ALT_P = 1.0
VEL_P = 1.0
VEL_I = 0
VEL_D = 0


if __name__ == '__main__':

    # initial conditions
    z = 0
    zprev = 0
    tprev = 0
    dzdt = 0
    u = 0

    # Create PID controller
    pid = AltitudePidController(ALTITUDE_TARGET, ALT_P, VEL_P, VEL_I, VEL_D)

    # Create a multicopter simulation
    copter = Multicopter()

    # Start the simulation
    copter.start()

    # Open a log file
    fp = open('ardupid.csv', 'w')

    print('Hit the start button ... ')
    stdout.flush()

    # Loop until user hits the stop button
    while True:

        # Wait until simulator starts up
        if not copter.isReady():
            continue

        # Get vehicle state from sim
        telem = copter.getState()

        # Extract time from state
        t = telem[0]

        # Negative time means user hit stop button
        if t < 0:
            break

        # Extract altitude from state.  Altitude is in NED coordinates, so we
        # negate it to use as input to PID controller.
        z = -telem[5]

        # Compute vertical climb rate as first difference of altitude/time
        if t > tprev:

            # Write time and altitude to log file
            if t <= 20.0:
                fp.write('%2.2f,%+3.3f\n' % (t, z))
                fp.flush()

            # Use temporal first difference to compute vertical velocity
            dt = t - tprev
            dzdt = (z-zprev) / dt

            # Get correction from PID controller
            u = pid.u(z, dzdt, dt)

            # Constrain correction to [0,1] to represent motor value
            u = max(0, min(1, u))

        # Set motor values in sim
        copter.setMotors(u*np.ones(4))

        # Update for first difference
        zprev = z
        tprev = t

        # Yield to Multicopter thread
        sleep(.001)
