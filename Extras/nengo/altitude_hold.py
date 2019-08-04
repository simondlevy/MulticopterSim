'''
Test simple altitude-hold PID controller

Copyright (C) 2019 Simon D. Levy

MIT License
'''

import numpy as np

# Target 
ALTITUDE_TARGET = 10

def runpid(copter, pid):

    # Get vehicle state from sim
    telem = copter.getState()

    # Extract time from state
    t =  telem[0]

    # Negative time means user hit stop button
    if t < 0: return False

    # Extract altitude from state.  Altitude is in NED coordinates, so we negate it to use as input
    # to PID controller.
    z = -telem[9]

    # Get correction from PID controller
    u = pid.getCorrection(ALTITUDE_TARGET, z)[0]

    # Constrain correction to [0,1] to represent motor value
    u = max(0, min(1, u))

    # Set motor values in sim
    copter.setMotors(u*np.ones(4))

    # Still runnin'
    return True
