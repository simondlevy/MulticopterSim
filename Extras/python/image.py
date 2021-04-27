#!/usr/bin/env python3
'''
Test image transmission

Copyright (C) 2021 Simon D. Levy

MIT License
'''

import socket
import numpy as np
import cv2
from time import sleep

# Comms
HOST = '127.0.0.1'
PORT = 5001

# Image size
ROWS = 480
COLS = 640


if __name__ == '__main__':

    # Serve a socket with a maximum of one client
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.bind((HOST, PORT))
    sock.listen(1)

    print('Server listening on %s:%d ... ' % (HOST, PORT))

    # This will block (wait) until a client connets
    conn, addr = sock.accept()

    print('Got a connection!')

    while True:

        imgbytes = conn.recv(ROWS*COLS*4)

        if len(imgbytes) == ROWS*COLS*4:

            rgba_image = np.reshape(np.frombuffer(imgbytes, 'uint8'), (ROWS, COLS, 4))

            image = cv2.cvtColor(rgba_image, cv2.COLOR_RGBA2BGR)

            cv2.imshow('Image', image)
            cv2.waitKey(1)
