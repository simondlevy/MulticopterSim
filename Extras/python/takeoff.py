#!/usr/bin/env python3
'''
Test simple altitude-hold PID controller

Copyright (C) 2019 Simon D. Levy

MIT License
'''

import numpy as np
import matplotlib.pyplot as plt
from pidcontroller import AltitudePidController
from multicopter import Multicopter

# Target params
ALTITUDE_START  = 0
ALTITUDE_TARGET = 10
VARIO_TOLERANCE = .01 # level-off velocity

# PID params
ALT_P = 1.25
VEL_P = 1.5
VEL_I = 1.0
VEL_D = 0.05

if __name__ == '__main__':

    # initial conditions
    z = 0
    zprev = 0
    tprev = 0
    dzdt = 0
    u = 0

    # Create PID controller
    pid  = AltitudePidController(ALTITUDE_TARGET, ALT_P, VEL_P, VEL_I, VEL_D)

    # Create a multicopter simulation
    copter = Multicopter()

    # Start the simulation
    copter.start()

    # Loop until level-off
    while True:

        # Get vehicle state from sim
        telem = copter.getState()

        # Extract time, altitude from state.  Altitude is in NED coordinates, so we negate it to use as input
        # to PID controller.
        t =  telem[0]
        z = -telem[9]

        print('%+5.5f,%+3.3f' % (t, z))

        # Compute vertical climb rate as first difference of altitude over time
        if t > tprev:

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

        # If altitude has leveled off, halt
        if abs(z) != 0 and abs(dzdt) < VARIO_TOLERANCE:
            break

    # Stop the simulation
    copter.stop()
