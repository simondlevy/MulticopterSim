'''
Simple altitude-hold and fly-forward

Copyright (C) 2019 Simon D. Levy

MIT License
'''


class LaunchController(object):

    def __init__(self, posP, velP, velI, windupMax=10):

        # Constants
        self.posP = posP
        self.velP = velP
        self.velI = velI
        self.windupMax = windupMax

        # Values modified in-flight
        self.integralError = 0
        self.tprev = 0

    def getDemands(self, target, alt, vel, t):

        # Compute dzdt setpoint and error
        velTarget = (target - alt) * self.posP
        velError = velTarget - vel

        # Compute dt
        dt = t - self.tprev

        # Update error integral and error derivative
        self.integralError += velError * dt
        self.integralError = LaunchController._constrainAbs(
                        self.integralError + velError * dt, self.windupMax)

        # Store time for first difference
        self.tprev = t

        # Always compute throttle demand for altitude hold
        throttle = self.velP * velError + self.velI * self.integralError

        # Don't mess with roll and yaw for now
        roll = 0
        yaw = 0

        # Pitch forward slightly for one second after level-off
        pitch = 0.001 if (2 < t < 3.25) else 0

        return throttle, roll, pitch, yaw

    def _constrainAbs(x, lim):

        return -lim if x < -lim else (+lim if x > +lim else x)
