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
        on_step.copter = Multicopter()
        on_step.copter.start()
        
    if on_step.copter.isReady():
        print(on_step.copter.getState())
        on_step.copter.setMotors(.5*np.ones(4))
