'''
  MultiCopter client class

  Copyright(C) 2023 Simon D.Levy

  MIT License
'''

from threading import Thread
import socket
import numpy as np
import sys
from time import sleep

try:
    import cv2
except Exception:
    pass


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

    def __init__(
            self,
            host='127.0.0.1',
            telemetry_port=5000,
            image_port=5002,
            image_rows=480,
            image_cols=640):

        self.host = host
        self.telemetry_port = telemetry_port
        self.image_port = image_port
        self.image_rows = image_rows
        self.image_cols = image_cols

        self.image = None

    def start(self):

        thread = Thread(target=self._run_thread)

        thread.start()

        # Serve a TCP socket for images socket with a maximum of one client
        imageServerSocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        imageServerSocket.setsockopt(
                socket.SOL_SOCKET, socket.SO_REUSEADDR, True)
        imageServerSocket.bind((self.host, self.image_port))
        imageServerSocket.listen(1)
        imageConn, _ = imageServerSocket.accept()
        imageConn.settimeout(1)

        while True:  # not self.done:

            try:
                sleep(.001)  # Yield to other thread

                try:
                    imgbytes = (imageConn.recv(
                        self.image_rows * self.image_cols * 4))

                except Exception:  # likely a timeout from sim quitting
                    break

                if len(imgbytes) == self.image_rows*self.image_cols*4:

                    rgba_image = np.reshape(np.frombuffer(imgbytes, 'uint8'),
                                            (self.image_rows,
                                             self.image_cols,
                                             4))

                    image = cv2.cvtColor(rgba_image, cv2.COLOR_RGBA2RGB)

                    self.handleImage(image)

            except KeyboardInterrupt:
                break

    def _run_thread(self):

        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:

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
