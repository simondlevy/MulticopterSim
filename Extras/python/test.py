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

    while True:

        telem = np.frombuffer(telemSocket.recvfrom(80)[0])

        print(telem)

        motorSocket.sendto(np.ndarray.tobytes(0.55*np.ones(4)), ('127.0.0.1', 5000))

