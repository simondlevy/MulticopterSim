#!/usr/bin/env python3

import socket
import numpy as np

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

    lastError = 0

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

            # Compute dzdt setpoint and error
            velTarget = (ALTITUDE_TARGET- z) * ALT_P
            velError = velTarget - dzdt

            # Update error integral and error derivative
            deltaError = (velError - lastError) / dt if abs(lastError) > 0 else 0

            lastError = velError

            # Compute control u
            u = VEL_P * velError + VEL_D * deltaError

            print('%+3.3f' % u)

            # Constrain correction to [0,1] to represent motor value
            u = max(0, min(1, u))

            #print('%5.5f,%5.5f,%+3.3f,%+3.3f,%+3.3f' % (t, dt, u, z, dzdt))

        tprev = t
        zprev = z
     
        motorSocket.sendto(np.ndarray.tobytes(u*np.ones(4)), (HOST, MOTOR_PORT))
