#!/usr/bin/env python3

import socket
import numpy as np

motorSocket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
motorSocket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, True)

while True:

    motorSocket.sendto(np.ndarray.tobytes(np.array([0.6,0.6,0.6,0.6])), ('127.0.0.1', 5000))



