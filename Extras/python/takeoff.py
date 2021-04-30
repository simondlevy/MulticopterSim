#!/usr/bin/env python3
'''
Test simple altitude-hold PID controller

Copyright (C) 2019 Simon D. Levy

MIT License
'''

from time import sleep
import numpy as np
from pidcontroller import AltitudePidController
from multicopter_sim import Multicopter


class TakeoffCopter(Multicopter):

    def __init__(self, altP=1.0, velP=1.0, velI=0.0, velD=0.0, target=10.0):

        Multicopter.__init(self)

        # Set up initial conditions
        self.z = 0
        self.zprev = 0
        self.tprev = 0
        self.dzdt = 0
        self.u = 0

        # Create PID controller
        pid = AltitudePidController(target, altP, velP, velD)

        # Open a log file
        self.logfile = open('ardupid.csv', 'w')

    def getMotors(telemetry):

        # Get vehicle state from sim
        t, x = copter.getTime(), copter.getState()

        # Negative time means user hit stop button
        if t < 0:
            break

        # Extract altitude from state.  Altitude is in NED coordinates, so we
        # negate it to use as input to PID controller.
        z = -x[4]

        # Compute vertical climb rate as first difference of altitude/time
        if t > tprev:

            # Write time and altitude to log file
            if t <= 20.0:
                self.logfile.write('%2.2f,%+3.3f\n' % (t, z))
                self.logfile.flush()

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
