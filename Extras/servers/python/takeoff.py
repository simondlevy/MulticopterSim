#!/usr/bin/env python3
'''
Test simple altitude-hold PID controller

Copyright (C) 2019 Simon D. Levy

MIT License
'''

import numpy as np
from altitude_controller import AltitudeController
from multicopter import Multicopter


class TakeoffCopter(Multicopter):

    def __init__(self, kp_z=1.0, kp_dz=1.0, ki_dz=0.0, target=10.0):

        Multicopter.__init__(self)

        # Set up initial conditions
        self.tprev = 0

        # Create PID controller
        self.ctrl = AltitudeController(target, kp_z, kp_dz, ki_dz)

        # Open a log file
        self.logfile = open('takeoff.csv', 'w')

    def getMotors(self, t, x):

        # Negative time means user hit stop button
        if t < 0:
            return

        # Extract altitude from state.  Altitude is in NED coordinates, so we
        # negate it to use as input to PID controller.
        z = -x[4]
        dzdt = -x[5]

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

            # Get correction from PID controller
            u = self.ctrl.u(z, dzdt, dt)

            # Constrain correction to [0,1] to represent motor value
            u = max(0, min(1, u))

        # Update for first difference
        self.tprev = t

        # Return motor values
        return u*np.ones(4)


def main():

    copter = TakeoffCopter()
    copter.start()


main()
