#!/usr/bin/env python3

import socket
import numpy as np
from pidcontroller import AltitudePidController

# Target params
ALTITUDE_START  = 0
ALTITUDE_TARGET = 10
VARIO_TOLERANCE = .01 # level-off velocity

# PID params
ALT_P = 1.25
VEL_P = 1.5
VEL_I = 1.0
VEL_D = 0.05

def makesock():
    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, True)
    return sock

if __name__ == '__main__':

    # initial conditions
    z = 0
    zprev = 0
    tprev = 0
    dzdt = 0
    u = 0

    # Create PID controller
    pid  = AltitudePidController(ALTITUDE_TARGET, ALT_P, VEL_P, VEL_I, VEL_D)

    motorSocket = makesock()

    telemSocket = makesock()

    telemSocket.bind(('127.0.0.1', 5001))

    u = 0

    while True:

        telem = np.frombuffer(telemSocket.recvfrom(80)[0])

        # Extract time, altitude from state.  Altitude is in NED coordinates, so we negate it to use as input
        # to PID controller.
        t =  telem[0]
        z = -telem[9]

        print('%+5.5f,%+3.3f,%+3.3f,%+3.3f' % (t, u, z, dzdt))

        # Compute vertical climb rate as first difference of altitude over time
        if t > tprev:

            # Use temporal first difference to compute vertical velocity
            dt = t - tprev
            dzdt = (z-zprev) / dt

            # Get correction from PID controller
            u = pid.u(z, dzdt, dt)

            # Constrain correction to [0,1] to represent motor value
            u = max(0, min(1, u))
     
        motorSocket.sendto(np.ndarray.tobytes(u*np.ones(4)), ('127.0.0.1', 5000))

