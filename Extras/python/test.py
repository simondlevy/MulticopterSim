#!/usr/bin/env python3

import socket
import numpy as np

motorSocket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
motorSocket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, True)

telemSocket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
telemSocket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, True)

telemSocket.bind(('127.0.0.1', 5001))

while True:

    motorSocket.sendto(np.ndarray.tobytes(0.55*np.ones(4)), ('127.0.0.1', 5000))

    telemSocket.recvfrom(80)

