'''
  Copyright(C) 2021 Simon D.Levy

  MIT License
'''

from threading import Thread
import socket
import numpy as np
from sys import stdout


# 12-dimensional state vector (Bouabdallah 2004)
STATE_SIZE = 12

HOST='127.0.0.1'
MOTOR_PORT=5000
TELEMETRY_PORT=5001
MOTOR_COUNT=4
TIMEOUT=.1

def debug(msg):
    print(msg)
    stdout.flush()

def run_telemetry(telemetryServerSocket, motorClientSocket):

    done = False
    running = False

    while True:

        try:
            data, _ = telemetryServerSocket.recvfrom(8*13)
        except Exception:
            done = True
            break

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


def make_udpsocket():

    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, True)

    return sock


def main():

    # Telemetry in and motors out run on their own thread
    motorClientSocket = make_udpsocket()
    telemetryServerSocket = make_udpsocket()
    telemetryServerSocket.bind((HOST, TELEMETRY_PORT))
    telemetryThread = Thread(target=run_telemetry,
                             args=(telemetryServerSocket, motorClientSocket))

    debug('Hit the start button ... ')
    telemetryThread.start()

main()
