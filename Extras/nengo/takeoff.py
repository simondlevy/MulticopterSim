#!/usr/bin/env python3
'''
Test simple altitude-hold PID controller

Copyright (C) 2019 Simon D. Levy

MIT License
'''

import nengo
from time import sleep
from multicopter_sim import Multicopter
from sys import stdout
from altitude_hold import runpid

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
        while runpid(copter, pid):

            # Update the simulator
            sim.run(SIM_TIME)

            # Yield to Multicopter thread
            sleep(.001)
