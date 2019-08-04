'''
Test simple altitude-hold PID controller in Nengo GUI

Copyright (C) 2019 Simon D. Levy

MIT License
'''

import nengo
import numpy as np

from multicopter_sim import Multicopter
from nengo_pid_controller import NengoPidController

# Target 
ALTITUDE_TARGET = 10

# PID params
KP = 0.4
KD = 10.0
KI = 0.03

model = nengo.Network()

with model:

    pid = NengoPidController(model, KP, KD, KI)

def on_step(sim):
    
    if not hasattr(on_step, 'copter'):

        # Start the 'copter
        on_step.copter = Multicopter()
        on_step.copter.start()
        
    if on_step.copter.isReady():

        # Get 'copter state from sim
        telem = on_step.copter.getState()

        # Extract altitude from state.  Altitude is in NED coordinates, so we negate it to use as input
        # to PID controller.
        z = -telem[9]

        # Get correction from PID controller
        u = pid.getCorrection(ALTITUDE_TARGET, z)[0]

        # Constrain correction to [0,1] to represent motor value
        u = max(0, min(1, u))

        # Set motor values in sim
        on_step.copter.setMotors(u*np.ones(4))
