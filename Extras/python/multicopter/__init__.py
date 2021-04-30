'''
  Socket-based multicopter class

  Copyright(C) 2021 Simon D.Levy

  MIT License
'''

from threading import Thread
import socket
import numpy as np
from sys import stdout
from time import sleep
import cv2


class Multicopter(object):

    def __init__(
            self,
            host='127.0.0.1',
            motor_port=5000,
            telem_port=5001,
            image_port=5002,
            image_rows=480,
            image_cols=640):

        self.host = host
        self.motor_port = motor_port
        self.telem_port = telem_port
        self.image_port = image_port
        self.image_rows = image_rows
        self.image_cols = image_cols

        self.telem = None
        self.image = None

    def start(self):

        done = [False]

        # Telemetry in and motors out run on their own thread
        motorClientSocket = Multicopter._make_udpsocket()
        telemetryServerSocket = Multicopter._make_udpsocket()
        telemetryServerSocket.bind((self.host, self.telem_port))
        telemetryThread = Thread(target=self._run_telemetry,
                                 args=(
                                       telemetryServerSocket,
                                       motorClientSocket,
                                       done))

        # Serve a socket with a maximum of one client
        imageServerSocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        imageServerSocket.bind((self.host, self.image_port))
        imageServerSocket.listen(1)

        Multicopter.debug('Hit the Start button ...')

        # This will block (wait) until a client connets
        imageConn, _ = imageServerSocket.accept()

        imageConn.settimeout(1)

        Multicopter.debug('Got a connection!')

        telemetryThread.start()

        while not done[0]:

            try:
                imgbytes = imageConn.recv(self.image_rows*self.image_cols*4)

            except Exception:  # likely a timeout from sim quitting
                break

            if len(imgbytes) == self.image_rows*self.image_cols*4:

                rgba_image = np.reshape(np.frombuffer(imgbytes, 'uint8'),
                                        (self.image_rows, self.image_cols, 4))

                image = cv2.cvtColor(rgba_image, cv2.COLOR_RGBA2RGB)

                cv2.imshow('Image', image)
                cv2.waitKey(1)

    def getMotors(self, telemtry):
        '''
        Override for your application
        '''
        return np.array([0.6, 0.6, 0.6, 0.6])

    @staticmethod
    def debug(msg):
        print(msg)
        stdout.flush()

    def _run_telemetry(self, telemetryServerSocket, motorClientSocket, done):

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
                Multicopter.debug('Running')
                running = True

            if telem[0] < 0:
                motorClientSocket.close()
                telemetryServerSocket.close()
                break

            motorvals = self.getMotors(telem)

            motorClientSocket.sendto(np.ndarray.tobytes(motorvals),
                                     (self.host, self.motor_port))

            sleep(.001)

    @staticmethod
    def _make_udpsocket():

        sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, True)

        return sock
