'''
  Socket-based multicopter class

  Copyright(C) 2021 Simon D.Levy

  MIT License
'''

from threading import Thread
import socket
import numpy as np
import sys
from time import sleep


class MulticopterClient(object):

    # See Bouabdallah (2004)
    (STATE_X,
     STATE_DX,
     STATE_Y,
     STATE_DY,
     STATE_Z,
     STATE_DZ,
     STATE_PHI,
     STATE_DPHI,
     STATE_THETA,
     STATE_DTHETA,
     STATE_PSI,
     STATE_DPSI) = range(12)

    def __init__(self, host='127.0.0.1', port=5000):

        self.host = host
        self.port = port

        self.sock = MulticopterClient._make_tcpsocket()

        self.done = False

    def start(self):

        thread = Thread(target=self._run_thread)

        thread.start()

        while not self.done:

            try:
                sleep(0)  # Yield to other thread

            except KeyboardInterrupt:
                self.done = True
                break

    def isDone(self):

        return self.done

    def _run_thread(self):

        self.sock.connect((self.host, self.port))

        while not self.done:

            try:
                telemetry_bytes = self.sock.recv(17 * 8); 

                telemetry = np.frombuffer(telemetry_bytes)

                print('%6.6f' % telemetry[-1])

                motorvals = self.getMotors(telemetry[0],     # time
                                           telemetry[1:13],  # vehicle state
                                           telemetry[13:])   # demands
                self.sock.send(
                        np.ndarray.tobytes(np.ndarray.astype(motorvals, np.float32)))

            except Exception as e:
                print('Exception: ' + str(e))
                self.done = True
                break

            sleep(0)  # yield to other thread

    @staticmethod
    def _make_tcpsocket():

        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, True)

        return sock
