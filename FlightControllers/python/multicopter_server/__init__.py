'''
   Socket-based multicopter class

   Copyright(C) 2021 Simon D.Levy

   This file is part of SimFlightControl.
 
   SimFlightControl is free software: you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the Free
   Software Foundation, either version 3 of the License, or (at your option)
   any later version.
 
   SimFlightControl is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
   more details.
 
   You should have received a copy of the GNU General Public License along with
   SimFlightControl. If not, see <https://www.gnu.org/licenses/>.

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


class MulticopterServer(object):

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
            dt=0,
            host='127.0.0.1',
            motor_port=5000,
            telemetry_port=5001,
            image_port=5002,
            image_rows=480,
            image_cols=640):

        # Constants
        self.dt = dt
        self.host = host
        self.motor_port = motor_port
        self.telemetry_port = telemetry_port
        self.image_port = image_port
        self.image_rows = image_rows
        self.image_cols = image_cols

        # State variables
        self.tprev = 0
        self.image = None
        self.done = False
        self.motorvals = np.zeros(4)

    def start(self):

        motorClientSocket = MulticopterServer._make_udpsocket()

        telemetryServerSocket = MulticopterServer._make_udpsocket()
        telemetryServerSocket.bind((self.host, self.telemetry_port))

        _debug('Hit the Play button ...')

        # Serve a TCP socket for images socket with a maximum of one client
        imageServerSocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        imageServerSocket.setsockopt(
                socket.SOL_SOCKET, socket.SO_REUSEADDR, True)
        imageServerSocket.bind((self.host, self.image_port))
        imageServerSocket.listen(1)
        imageConn, _ = imageServerSocket.accept()
        imageConn.settimeout(1)

        thread = Thread(target=self._run_thread,
                        args=(telemetryServerSocket, motorClientSocket))

        thread.start()

        while not self.done:

            try:
                sleep(0)  # Yield to other thread

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

    def handleImage(self, image):
        '''
        Override for your application
        '''
        try:
            cv2.imshow('Image', image)
            cv2.waitKey(1)
        except Exception:
            pass

    def getMotors(self, dt, state, demands):
        '''
        Override for your application.  Should return motor values in interval
        [0,1].  This default implementation just keeps flying upward.
        '''
        return 0.6 * np.ones(4)

    def isDone(self):

        return self.done

    def _run_thread(self, telemetryServerSocket, motorClientSocket):

        running = False

        while True:

            try:
                telemetry_bytes, _ = telemetryServerSocket.recvfrom(8*17)
            except Exception:
                self.done = True
                break

            telemetryServerSocket.settimeout(.1)

            telemetry = np.frombuffer(telemetry_bytes)

            if not running:
                _debug('Running')
                running = True

            # Sim sends -1 for time when it quits
            t = telemetry[0]
            if t < 0:
                self.done = True
                break

            # Update motors values periodically
            dt = t - self.tprev

            if dt > self.dt:

                self.tprev = t

                self.motorvals = self.getMotors(dt,
                                           telemetry[1:13],  # vehicle state
                                           telemetry[16:])   # demands

            # Always send current motor values to avoid starving the sim
            motorClientSocket.sendto(
                    np.ndarray.tobytes(np.ndarray.astype(self.motorvals, np.float32)),
                    (self.host, self.motor_port))

            sleep(0)  # yield to other thread

    @staticmethod
    def _make_udpsocket():

        sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, True)

        return sock
