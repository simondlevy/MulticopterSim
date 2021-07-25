'''
  Socket-based multicopter class

  Copyright(C) 2021 Simon D.Levy

  MIT License
'''

from threading import Thread
import socket
import numpy as np
import sys
import time
import cv2


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
            host='127.0.0.1',
            motor_port=5000,
            telemetry_port=5001,
            image_port=5002,
            image_rows=480,
            image_cols=640):

        self.host = host
        self.motor_port = motor_port
        self.telemetry_port = telemetry_port
        self.image_port = image_port
        self.image_rows = image_rows
        self.image_cols = image_cols

        self.telem = None
        self.image = None
        self.done = False

    def start(self):

        motorClientSocket = MulticopterServer._make_udpsocket()

        telemetryServerSocket = MulticopterServer._make_udpsocket()
        telemetryServerSocket.bind((self.host, self.telemetry_port))

        _debug('Hit the Play button ...')

        thread = Thread(target=self._run_threadetry,
                        args=(telemetryServerSocket,
                              motorClientSocket))

        # Serve a socket with a maximum of one client
        imageServerSocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        imageServerSocket.bind((self.host, self.image_port))
        imageServerSocket.listen(1)

        # This will block (wait) until a client connets
        imageConn, _ = imageServerSocket.accept()
        imageConn.settimeout(1)
        _debug('Got a connection!')

        thread.start()

        while not self.done:

            try:
                imgbytes = imageConn.recv(self.image_rows*self.image_cols*4)

            except Exception:  # likely a timeout from sim quitting
                break

            if len(imgbytes) == self.image_rows*self.image_cols*4:

                rgba_image = np.reshape(np.frombuffer(imgbytes, 'uint8'),
                                        (self.image_rows, self.image_cols, 4))

                image = cv2.cvtColor(rgba_image, cv2.COLOR_RGBA2RGB)

                self.handleImage(image)

    def handleImage(self, image):
        '''
        Override for your application
        '''
        cv2.imshow('Image', image)
        cv2.waitKey(1)

    def getMotors(self, time, state, demands):
        '''
        Override for your application
        '''
        return np.array([0.6, 0.6, 0.6, 0.6])

    def isDone(self):

        return self.done

    def _run_threadetry(self,
                        telemetryServerSocket,
                        motorClientSocket):

        running = False

        while True:

            try:
                telemetry_bytes, _ = telemetryServerSocket.recvfrom(8*17)
            except Exception:
                self.done = True
                _debug('EXCEPTION')
                break

            telemetryServerSocket.settimeout(.1)

            telemetry = np.frombuffer(telemetry_bytes)

            if not running:
                _debug('Running')
                running = True

            if telemetry[0] < 0:
                motorClientSocket.close()
                telemetryServerSocket.close()
                break

            motorvals = self.getMotors(telemetry[0],     # time
                                       telemetry[1:13],  # vehicle state
                                       telemetry[13:])   # demands

            motorClientSocket.sendto(np.ndarray.tobytes(motorvals),
                                     (self.host, self.motor_port))

            time.sleep(.001)

    @staticmethod
    def _make_udpsocket():

        sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, True)

        return sock
