#!/usr/bin/env python3
'''
Test simple altitude-hold PID controller

Copyright (C) 2019 Simon D. Levy

MIT License
'''

import cv2
import numpy as np
from launch_controller import LaunchController
from multicopter import Multicopter


class LaunchCopter(Multicopter):

    def __init__(
            self,
            kp_z=1.0,
            kp_dz=1.0,
            ki_dz=0.0,
            initial_target=15.0):

        Multicopter.__init__(self)

        self.time = 0
        self.target = initial_target

        # Create PID controller
        self.ctrl = LaunchController(kp_z, kp_dz, ki_dz)

    def handleImage(self, image):
        gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
        edges = cv2.Canny(gray, 100, 200)
        cv2.imshow('Edge Detection', edges)
        cv2.waitKey(1)

        nonzero = np.nonzero(edges)

        # Ignore image for first five seconds
        if np.mean(nonzero[0]) > 390 and self.time > 5:
            self.target = 30

    def getMotors(self, t, state):

        # Negative time means user hit stop button
        if t < 0:
            return

        # Track current time to share it with handleImage()
        self.time = t

        # Extract altitude from state.  Altitude is in NED coordinates, so we
        # negate it to use as input to PID controller.
        z = -state[Multicopter.STATE_Z]
        dzdt = -state[Multicopter.STATE_DZ]

        # No demands yet
        u = 0

        # Get demands U [throttle, roll, pitch, yaw] from PID controller
        u = self.ctrl.getDemands(self.target, z, dzdt, t)

        motors = self.mixer(u)

        # Constrain correction to [0,1] to represent motor value
        motors[motors > 1] = 1
        motors[motors < 0] = 0

        # Return motor values
        return motors

    def mixer(self, u):
        '''
        Converts demands U [throttle, roll, pitch, yaw] into four motor demands
        Omega
        '''

        #                      Th  RR  PF  YR
        d = ((+1, -1, -1, +1),   # 1 right front
             (+1, +1, +1, +1),   # 2 left rear
             (+1, +1, -1, -1),   # 3 left front
             (+1, -1, +1, -1))   # 4 right rear

        motorvals = np.zeros(4)

        for i in range(4):
            motorvals[i] = (u[0] * d[i][0] + u[1] * d[i][1] +
                            u[2] * d[i][2] + u[3] * d[i][3])

        return motorvals


def main():

    copter = LaunchCopter()
    copter.start()


main()
