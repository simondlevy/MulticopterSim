#!/usr/bin/env python
'''
  Climb-and-hold-altitude example using Multicopter class

  Copyright(C) 2019 Simon D.Levy

  MIT License
'''

from multicopter import Multicopter
from altitude_hold import AltitudePidController
import numpy as np

# Altitude target (meters)
ALTITUDE_TARGET = 10

# We stop when altitude change falls below this value
ALTITUDE_TOLERANCE = 1E-5

# Time constant
DT = 0.001

# PID params
ALT_P = 0 #5
VEL_P = 0 #1.5
VEL_I = 0 #1.0
VEL_D = 0 #0.05

# Create out altitude-hold PID controller
pid = AltitudePidController(-ALTITUDE_TARGET, ALT_P, VEL_P, VEL_I, VEL_D)

# Create a multicopter simulation
copter = Multicopter()

# Start the simulation
copter.start()

# Initialize previous Z (altitude) value, to check level-off
zprev = 0

# Start with zero throttle
u = np.zeros(4)

t = 0

# Run until altitude stabilizes
while True:

    # Set motor values in sim
    copter.setMotors(u)

    # Get telemetry from sim
    telem = copter.getState()

    # Extract Z (altitude) from telemetry
    z = -telem[10]

    dz = z - zprev

    print('t:%3.3f u:%3.3f z:%+8.8f zprev:%+8.8f dz:%f' % (t, u[0], z, zprev, dz))

    # If altitude has leveled off, halt
    #if z > 1 and abs(dz) < ALTITUDE_TOLERANCE:
    #    break

    # Update PID controller with Z, dZ/dt, and dt, getting correction
    u = pid.u(z, dz/DT, DT) * np.ones(4)

    t += DT

    # Use correct to set motor values
    copter.setMotors(np.ones(4))

    # Track previous Z
    zprev = z

# Stop the simulation
copter.stop()
