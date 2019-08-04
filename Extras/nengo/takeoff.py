#!/usr/bin/env python3
'''
Test simple altitude-hold PID controller

Copyright (C) 2019 Simon D. Levy

MIT License
'''

import nengo
from time import sleep
import numpy as np
from multicopter_sim import Multicopter
from sys import stdout

# Target 
ALTITUDE_TARGET = 10

# PID params
KP = 0.4
KD = 10.0
KI = 0.03

# Simulator params
SIM_TIME = 0.001

def debug(msg):

    stdout.write(msg)
    stdout.flush()

if __name__ == '__main__':

    # initial conditions
    z = 0
    u = 0

    debug('Importing Nengo ... ')
    from nengo_pid_controller import NengoPidController
    debug('done\nHit the Play button ...')

    model = nengo.Network()

    # Create PID controller
    pid = NengoPidController(model, KP, KD, KI)

    sim = nengo.Simulator(model, progress_bar=False)  

    with model:

        # Create a multicopter simulation
        copter = Multicopter()

        # Start the simulation
        copter.start()

        # Loop until user hits the stop button
        while True:

            # Get vehicle state from sim
            telem = copter.getState()

            # Extract time from state
            t =  telem[0]

            # Negative time means user hit stop button
            if t < 0: break

            # Extract altitude from state.  Altitude is in NED coordinates, so we negate it to use as input
            # to PID controller.
            z = -telem[9]

            # Compute vertical climb rate as first difference of altitude over time
            if copter.isReady():

                # Get correction from PID controller
                u = pid.getCorrection(ALTITUDE_TARGET, z)[0]

                # Update the simulator
                sim.run(SIM_TIME)

                # Constrain correction to [0,1] to represent motor value
                u = max(0, min(1, u))

            # Set motor values in sim
            copter.setMotors(u*np.ones(4))

            # Yield to Multicopter thread
            sleep(.001)
