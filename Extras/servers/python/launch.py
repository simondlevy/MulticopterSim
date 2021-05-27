#!/usr/bin/env python3
'''
Simple take-off-and-move-forward script

Copyright (C) 2021 Simon D. Levy

MIT License
'''

import cv2
import numpy as np
import argparse
from argparse import ArgumentDefaultsHelpFormatter

from launch_controller import LaunchController
from multicopter_server import MulticopterServer
from mixers import PhantomMixer, IngenuityMixer


class LaunchCopter(MulticopterServer):

    def __init__(
            self,
            mixer,
            kp_z=1.0,
            kp_dz=1.0,
            ki_dz=0.0,
            initial_target=15.0):

        MulticopterServer.__init__(self)

        self.mixer = mixer

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
        z = -state[MulticopterServer.STATE_Z]
        dzdt = -state[MulticopterServer.STATE_DZ]

        # No demands yet
        u = 0

        # Get demands U [throttle, roll, pitch, yaw] from PID controller
        u = self.ctrl.getDemands(self.target, z, dzdt, t)

        # Use mixer to convert demands U into motor values Omega
        omega = self.mixer.getMotors(u)

        # Constrain motor values to [0,1]
        omega[omega > 1] = 1
        omega[omega < 0] = 0

        # Return motor values
        return omega


def main():

    parser = argparse.ArgumentParser(
            formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument('vehicle',
                        help='Vehicle name [Phantom or Ingenuity]')

    args = parser.parse_args()

    d = {'Phantom': PhantomMixer, 'Ingenuity': IngenuityMixer}

    if args.vehicle in d:
        copter = LaunchCopter(d[args.vehicle]())
        copter.start()

    else:
        print('Unrecognized vehicle: %s' % args.vehicle)


main()
