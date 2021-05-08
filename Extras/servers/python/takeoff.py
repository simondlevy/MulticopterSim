#!/usr/bin/env python3
'''
Test simple altitude-hold PID controller

Copyright (C) 2019 Simon D. Levy

MIT License
'''

import numpy as np
from pidcontroller import AltitudePidController
from multicopter import Multicopter


class TakeoffCopter(Multicopter):

    def __init__(self, altP=1.0, velP=1.0, velI=0.0, velD=0.0, target=10.0):

        Multicopter.__init__(self)

        # Set up initial conditions
        self.z = 0
        self.zprev = 0
        self.tprev = 0
        self.dzdt = 0
        self.u = 0

        # Create PID controller
        self.pid = AltitudePidController(target, altP, velP, velI, velD)

        # Open a log file
        self.logfile = open('ardupid.csv', 'w')

    def getMotors(self, t, x):

        # Negative time means user hit stop button
        if t < 0:
            return

        # Extract altitude from state.  Altitude is in NED coordinates, so we
        # negate it to use as input to PID controller.
        z = -x[4]

        # No motor values yet
        u = 0

        # Compute vertical climb rate as first difference of altitude/time
        if t > self.tprev:

            # Write time and altitude to log file
            if t <= 20.0:
                self.logfile.write('%2.2f,%+3.3f\n' % (t, z))
                self.logfile.flush()

            # Use temporal first difference to compute vertical velocity
            dt = t - self.tprev
            dzdt = (z-self.zprev) / dt

            # Get correction from PID controller
            u = self.pid.u(z, dzdt, dt)

            # Constrain correction to [0,1] to represent motor value
            u = max(0, min(1, u))

        # Update for first difference
        self.zprev = z
        self.tprev = t

        # Return motor values
        return u*np.ones(4)


def main():

    copter = TakeoffCopter()
    copter.start()


main()
