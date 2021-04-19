'''
  Python Multicopter class

  Uses UDP sockets to communicate with MulticopterSim

  Copyright(C) 2019 Simon D.Levy

  MIT License
'''

from threading import Thread
import socket
import numpy as np


class Multicopter(object):
    '''
    Represents a Multicopter object communicating with MulticopterSim via UDP
    socket calls.
    '''

    # Time, followed by 12-dimensional state vector (Bouabdallah 2004)
    STATE_SIZE = 13

    def __init__(self, host='127.0.0.1', motorPort=5000, telemetryPort=5001,
                 motorCount=4, timeout=.1):
        '''
        Creates a Multicopter object.
        host - name of host running MulticopterSim
        motorPort - port over which this object will send motor commands to
                    host
        telemeteryPort - port over which this object will receive telemetry
                         from host
        motorCount - number of motors in vehicle running in simulator on host
        '''

        self.motorSocket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        self.motorSocket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR,
                                    True)

        self.telemSocket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        self.telemSocket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR,
                                    True)

        self.telemSocket.bind((host, telemetryPort))

        self.host = host
        self.motorPort = motorPort
        self.motorCount = motorCount

        self.thread = Thread(target=self._run)
        self.thread.daemon = True

        self.motorVals = np.zeros(motorCount)
        self.state = np.zeros(self.STATE_SIZE)

        self.timeout = timeout
        self.ready = False
        self.done = False

    def start(self):
        '''
        Begins communication with simulator running on host.
        '''

        self.thread.start()

    def isReady(self):

        if self.ready:
            self.telemSocket.settimeout(self.timeout)
        return self.ready

    def isDone(self):

        return self.done

    def getState(self):
        '''
        Returns current vehicle state
        '''

        return self.state

    def setMotors(self, motorVals):
        '''
        Sets motor values between 0 and 1.
        '''

        self.motorVals = np.copy(motorVals)

    def _run(self):

        self.done = False

        while True:

            try:
                data, _ = self.telemSocket.recvfrom(8*self.STATE_SIZE)
            except Exception:
                self.done = True
                break

            self.state = np.frombuffer(data)

            self.ready = True

            if self.state[0] < 0:
                self.motorSocket.close()
                self.telemSocket.close()
                break

            self.motorSocket.sendto(np.ndarray.tobytes(self.motorVals),
                                    (self.host, self.motorPort))
