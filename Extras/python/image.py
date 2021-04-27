#!/usr/bin/env python3
'''
Test image transmission

Copyright (C) 2021 Simon D. Levy

MIT License
'''

from sys import stdout
import socket
import numpy as np
import cv2

# Comms
HOST = '127.0.0.1'
PORT = 5002

# Image size
ROWS = 48
COLS = 64


def dump(msg):
    print(msg)
    stdout.flush()


if __name__ == '__main__':

    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, True)
    sock.bind((HOST, PORT))
    sock.settimeout(.01)

    while True:

        imgbytes = None

        try:
            imgbytes, _ = sock.recvfrom(ROWS*COLS*4)

        except Exception:
            pass

        if imgbytes is not None:

            rgba_image = np.reshape(np.frombuffer(imgbytes, 'uint8'), (ROWS, COLS, 4))

            image = cv2.cvtColor(rgba_image, cv2.COLOR_RGBA2BGR)

            cv2.imshow('Image', image)
            cv2.waitKey(1)
