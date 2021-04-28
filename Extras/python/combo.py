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
IMAGE_ROWS = 480
IMAGE_COLS = 640


def dump(msg):
    print(msg)
    stdout.flush()


def make_udpsocket():

    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, True)

    return sock


if __name__ == '__main__':

    # Serve a socket for images, with a maximum of one client
    image_server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    image_server_socket.bind((HOST, IMAGE_PORT))
    image_server_socket.listen(1)
    dump('Server listening on %s:%d ... ' % (HOST, IMAGE_PORT))

    motor_client_socket = make_udpsocket()
    telemetry_server_socket = make_udpsocket()

    # This will block (wait) until a client connets
    imgconn, _ = image_server_socket.accept()

    imgconn.settimeout(1)

    dump('Got a connection!')

    while True:

        try:
            imgbytes = imgconn.recv(IMAGE_ROWS*IMAGE_COLS*4)

        except Exception:  # likely a timeout from sim quitting
            break

        if len(imgbytes) == IMAGE_ROWS*IMAGE_COLS*4:

            rgba_image = np.reshape(np.frombuffer(imgbytes, 'uint8'),
                                    (IMAGE_ROWS, IMAGE_COLS, 4))

            image = cv2.cvtColor(rgba_image, cv2.COLOR_RGBA2RGB)

            cv2.imshow('Image', image)
            cv2.waitKey(1)
