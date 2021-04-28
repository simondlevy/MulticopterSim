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
PORT = 5002

# Image size
ROWS = 480
COLS = 640


def dump(msg):
    print(msg)
    stdout.flush()

def run(done):

    # Serve a socket with a maximum of one client
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.bind((HOST, PORT))
    sock.listen(1)

    dump('Server listening on %s:%d ... ' % (HOST, PORT))

    # This will block (wait) until a client connets
    conn, addr = sock.accept()

    conn.settimeout(1)

    dump('Got a connection!')

    while True:

        try:
            imgbytes = conn.recv(ROWS*COLS*4)

        except Exception:  # likely a timeout from sim quitting
            done[0] = True
            break

        if len(imgbytes) == ROWS*COLS*4:

            rgba_image = np.reshape(np.frombuffer(imgbytes, 'uint8'),
                                    (ROWS, COLS, 4))

            image = cv2.cvtColor(rgba_image, cv2.COLOR_RGBA2RGB)

            cv2.imshow('Image', image)
            cv2.waitKey(1)

if __name__ == '__main__':

    done = [False]

    thread = Thread(target=run, args=(done,))
    thread.start()

    while not done[0]:
        sleep(.001)


