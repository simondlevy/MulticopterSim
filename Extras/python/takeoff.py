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

# Can't touch this!
G = 9.80665

# Reasonable time constant
DT = 0.001

# Target params
ALTITUDE_START  = 0
ALTITUDE_TARGET = 10
ALTITUDE_TOLERANCE = .0001 # level-off velocity

# PID params
ALT_P = 1.25
VEL_P = 1.5
VEL_I = 1.0
VEL_D = 0.05

# Vehicle params
MAXRPM = 30000
B      = 5.30216718361085E-05 # Thrust coefficient
M      = 16.47                # Mass (kg)

# Plots results from CSV log file
def plot(logfilename):

    data = np.genfromtxt(logfilename, delimiter=',', skip_header=1)

    t = data[:,0]
    z = data[:,3]

    plt.plot(t, z)
    plt.xlabel('time (sec)')
    plt.ylabel('altitude (m)')
    plt.ylim([min(z)-1, max(z)+1])
    plt.show()

if __name__ == '__main__':

    # initial conditions
    t     = 0
    z     = ALTITUDE_START
    dzdt  = 0
    u     = 0

    # make CSV file name from these params
    filename = '%04.f-%04.f_%3.3f-%3.3f-%3.3f-%3.3f.csv' % (ALTITUDE_START, ALTITUDE_TARGET, ALT_P, VEL_P, VEL_I, VEL_D)
    logfile = open(filename, 'w')
    logfile.write('t,dzdt2,dzdtz,zs,u\n')

    # Create PID controller
    pid  = AltitudePidController(ALTITUDE_TARGET, ALT_P, VEL_P, VEL_I, VEL_D)

    # Create a multicopter simulation
    copter = Multicopter()

    # Start the simulation
    copter.start()

    # Loop until level-off
    while True:

        # If altitude has leveled off, halt
        if abs(z) != 0 and abs(dzdt) < ALTITUDE_TOLERANCE:
            break

        # Get correction from PID controller
        u = pid.u(z, dzdt, DT)

        # Constrain correction to [0,1] to represent motor value
        u = max(0, min(1, u))
 
        # Set motor values in sim
        copter.setMotors(u*np.ones(4))

        # Get vehicle state from sim
        telem = copter.getState()

        # Convert motor value to vertical thrust
        thrust = B * (u * MAXRPM * np.pi / 30) ** 2 / M

        # Subtract G from thrust to get net vertical acceleration
        dzdt2 = thrust - G

        # Integrate net vertical acceleration to get vertical velocity
        dzdt += dzdt2 * DT

        # Integrate vertical velocity to get altitude
        z += dzdt*DT

        # Write to log file
        logfile.write('%3.3f,%3.3f,%3.3f,%3.3f,%3.3f,%3.3f\n' % (t, dzdt2, dzdt, z, telem[10], u))

        # Accumulate time for logging
        t += DT

    # Stop the simulation
    copter.stop()

    logfile.close()

    plot(filename)
