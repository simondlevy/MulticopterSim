'''
   Helper functions for based altitude-hold PID controller using Nengo

   Copyright (C) 2019 Simon D. Levy

   This file is part of SimFlightControl.
 
   SimFlightControl is free software: you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the Free
   Software Foundation, either version 3 of the License, or (at your option)
   any later version.
 
   SimFlightControl is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
   more details.
 
   You should have received a copy of the GNU General Public License along with
   SimFlightControl. If not, see <https://www.gnu.org/licenses/>.

'''

import nengo
import numpy as np
from nengo_pid_controller import NengoPidController
from multicopter_sim import Multicopter

# Target
ALTITUDE_TARGET = 10

# PID params
KP = 0.4
KD = 10.0
KI = 0.03


def buildpid():

    # Create Nengo network
    model = nengo.Network()

    # Create PID controller
    pid = NengoPidController(model, KP, KD, KI)

    return model, pid


def startcopter():

    # Create a multicopter simulation
    copter = Multicopter()

    # Start the simulation
    copter.start()

    return copter


def runpid(copter, pid):

    # Get vehicle state from sim
    telem = copter.getState()

    # Extract altitude from state.  
    z = telem[5]

    # Get correction from PID controller
    u = pid.getCorrection(ALTITUDE_TARGET, z)[0]

    # Constrain correction to [0,1] to represent motor value
    u = max(0, min(1, u))

    # Set motor values in sim
    copter.setMotors(u*np.ones(4))
