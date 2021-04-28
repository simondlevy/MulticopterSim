#!/usr/bin/env python3
'''
Test image transmission

Copyright (C) 2021 Simon D. Levy

MIT License
'''

import socket
import numpy as np
import cv2
from sys import stdout
from threading import Thread
from time import sleep

# Comms
HOST = '127.0.0.1'
IMAGE_PORT = 5002

# Image size
ROWS = 480
COLS = 640


def dump(msg):
    print(msg)
    stdout.flush()

if __name__ == '__main__':

    # Serve a socket with a maximum of one client
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.bind((HOST, IMAGE_PORT))
    sock.listen(1)

    dump('Server listening on %s:%d ... ' % (HOST, IMAGE_PORT))

    # This will block (wait) until a client connets
    conn, _ = sock.accept()

    conn.settimeout(1)

    dump('Got a connection!')

    while True:

        try:
            imgbytes = conn.recv(ROWS*COLS*4)

        except Exception:  # likely a timeout from sim quitting
            break

        if len(imgbytes) == ROWS*COLS*4:

            rgba_image = np.reshape(np.frombuffer(imgbytes, 'uint8'),
                                    (ROWS, COLS, 4))

            image = cv2.cvtColor(rgba_image, cv2.COLOR_RGBA2RGB)

            cv2.imshow('Image', image)
            cv2.waitKey(1)
