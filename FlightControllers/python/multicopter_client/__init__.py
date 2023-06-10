'''
  MultiCopter client class

  Copyright(C) 2023 Simon D.Levy

  MIT License
'''

import socket
import numpy as np


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

    def __init__(
            self,
            host='127.0.0.1',
            telemetry_port=5000):

        self.host = host
        self.telemetry_port = telemetry_port

    def start(self):

        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:

            done = False

            try:

                sock.connect((self.host, self.telemetry_port))

                while True:

                    telemetry_bytes = sock.recv(8*17)

                    telemetry = np.frombuffer(telemetry_bytes)

                    # Server sends -1 on quit
                    if telemetry[0] == -1:
                        break

                    motorvals = self.getMotors(
                            telemetry[0],     # time
                            telemetry[1:13],  # vehicle state
                            telemetry[13:])   # demands

                    sock.send(np.ndarray.tobytes(motorvals))

            except ConnectionRefusedError:

                print('Connection error; did you start the server first?')

            except KeyboardInterrupt:

                exit(0)

    def getMotors(self, time, state, demands):
        '''
        Override for your application.  Should return motor values in interval
        [0,1].  This default implementation just keeps flying upward.
        '''
        return np.array([0.6, 0.6, 0.6, 0.6])
