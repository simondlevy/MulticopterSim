'''
  Copyright(C) 2021 Simon D.Levy

  MIT License
'''

from threading import Thread
import socket
import numpy as np
from sys import stdout
from time import sleep
import cv2


# 12-dimensional state vector (Bouabdallah 2004)
STATE_SIZE = 12

HOST ='127.0.0.1'
MOTOR_PORT = 5000
TELEM_PORT = 5001
IMAGE_PORT = 5002
IMAGE_ROWS = 480
IMAGE_COLS = 640

def debug(msg):
    print(msg)
    stdout.flush()

def run_telemetry(telemetryServerSocket, motorClientSocket, done):

    running = False

    while True:

        try:
            data, _ = telemetryServerSocket.recvfrom(8*13)
        except Exception:
            done[0] = True
            break

        telemetryServerSocket.settimeout(.01)

        telem = np.frombuffer(data)

        if not running:
            debug('Running')
            running = True

        if telem[0] < 0:
            motorClientSocket.close()
            telemetryServerSocket.close()
            break

        motorVals = np.array([0.6,0.6,0.6,0.6])

        motorClientSocket.sendto(np.ndarray.tobytes(motorVals),
                                (HOST, MOTOR_PORT))

        sleep(.001)


def make_udpsocket():

    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, True)

    return sock


def main():

    done = [False]

    # Telemetry in and motors out run on their own thread
    motorClientSocket = make_udpsocket()
    telemetryServerSocket = make_udpsocket()
    telemetryServerSocket.bind((HOST, TELEM_PORT))
    telemetryThread = Thread(target=run_telemetry,
                             args=(telemetryServerSocket, motorClientSocket, done))

    # Serve a socket with a maximum of one client
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.bind((HOST, IMAGE_PORT))
    sock.listen(1)

    debug('Hit the Start button ...')

    # This will block (wait) until a client connets
    conn, _ = sock.accept()

    conn.settimeout(1)

    debug('Got a connection!')

    telemetryThread.start()

    while not done[0]:

        try:
            imgbytes = conn.recv(IMAGE_ROWS*IMAGE_COLS*4)

        except Exception:  # likely a timeout from sim quitting
            break

        if len(imgbytes) == IMAGE_ROWS*IMAGE_COLS*4:

            rgba_image = np.reshape(np.frombuffer(imgbytes, 'uint8'),
                                    (IMAGE_ROWS, IMAGE_COLS, 4))

            image = cv2.cvtColor(rgba_image, cv2.COLOR_RGBA2RGB)

            cv2.imshow('Image', image)
            cv2.waitKey(1)
main()
