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


def _debug(msg):
    print(msg)
    sys.stdout.flush()


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

    def __init__(self, host='127.0.0.1', telem_port=5000):

        self.host = host
        self.telem_port = telem_port

        self.telem_sock = MulticopterClient._make_tcpsocket()

        self.done = False

    def start(self):

        thread = Thread(target=self._run_thread)

        thread.start()

        while not self.done:

            try:
                sleep(0)  # Yield to other thread

            except KeyboardInterrupt:
                break

    def getMotors(self, time, state, demands):
        '''
        Override for your application.  Should return motor values in interval
        [0,1].  This default implementation just keeps flying upward.
        '''
        return np.array([0.6, 0.6, 0.6, 0.6])

    def isDone(self):

        return self.done

    def _run_thread(self):

        self.telem_sock.connect((self.host, self.telem_port))

        while True:

            try:
                telemetry_bytes, _ = self.telem_sock.recvfrom(8*17)
            except Exception:
                self.done = True
                break

            telemetry = np.frombuffer(telemetry_bytes)

            # Server sends -1 for time when done
            t = telemetry[0]
            if t < 0:
                self.done = True
                break

            print(t)

            motorvals = self.getMotors(t,                # time
                                       telemetry[1:13],  # vehicle state
                                       telemetry[13:])   # demands

            sleep(0)  # yield to other thread

    @staticmethod
    def _make_tcpsocket():

        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, True)

        return sock
