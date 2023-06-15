'''
Simple altitude-hold and fly-forward

Copyright (C) 2019 Simon D. Levy

MIT License
'''


class LaunchController(object):

    def __init__(self, Kp):

        # Constants
        self.Kp = Kp

    def getDemands(self, target, alt, vel):

        # Compute dzdt setpoint and error
        velError = (target - alt) - vel

        # Always compute throttle demand for altitude hold
        throttle = self.Kp * velError

        # Don't mess with roll,pitch, and yaw for this simple demo
        roll = 0
        pitch = 0
        yaw = 0

        return throttle, roll, pitch, yaw
