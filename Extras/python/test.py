#!/usr/bin/env python3

import socket
import numpy as np
from pidcontroller import AltitudePidController

# Comms
HOST = '127.0.0.1'
MOTOR_PORT = 5000
TELEM_PORT = 5001

# Target params
ALTITUDE_TARGET = 10
VARIO_TOLERANCE = .05

# PID params
ALT_P = 1.0
VEL_P = 1.0
VEL_I = 0
VEL_D = 0

def makesock():
    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, True)
    return sock

if __name__ == '__main__':

    # initial conditions
    zprev = 0
    tprev = 0

    motorSocket = makesock()

    telemSocket = makesock()

    telemSocket.bind((HOST, TELEM_PORT))

    # Create a PID controller for altitude hold
    pid = AltitudePidController(ALTITUDE_TARGET, ALT_P, VEL_P, VEL_I, VEL_D)

    while True:

        telem = np.frombuffer(telemSocket.recvfrom(80)[0])

        # Extract time, altitude from state.  Altitude is in NED coordinates, so we negate it to use as input
        # to PID controller.
        t =  telem[0]
        z = -telem[9]

        # Compute vertical climb rate as first difference of altitude over time
        if t > tprev:

            # Use temporal first difference to compute vertical velocity
            dt = t - tprev
            dzdt = (z-zprev) / dt

            # Compute control u
            u = pid.u(z, dzdt, dt)

            # Constrain correction to [0,1] to represent motor value
            u = max(0, min(1, u))

        # Update previous time, altitude for PID controller
        tprev = t
        zprev = z
     
        motorSocket.sendto(np.ndarray.tobytes(u*np.ones(4)), (HOST, MOTOR_PORT))
