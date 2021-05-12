'''
Simple altitude-hold controller based on APM

Copyright (C) 2019 Simon D. Levy

MIT License
'''


class AltitudeController(object):

    def __init__(self, target, Kp_pos, Kp_vel, Ki_vel, windupMax=10):

        # In a real PID controller, this would be a set-point
        self.target = target

        # Constants
        self.Kp_pos = Kp_pos
        self.Kp_vel = Kp_vel
        self.Ki_vel = Ki_vel
        self.windupMax = windupMax

        # Values modified in-flight
        self.integralError = 0

    def u(self, alt, vel, dt):

        # Compute dzdt setpoint and error
        velTarget = (self.target - alt) * self.Kp_pos
        velError = velTarget - vel

        # Update error integral and error derivative
        self.integralError += velError * dt
        self.integralError = AltitudeController._constrainAbs(
                        self.integralError + velError * dt, self.windupMax)

        # Compute control u
        return self.Kp_vel * velError + self.Ki_vel * self.integralError

    def _constrainAbs(x, lim):

        return -lim if x < -lim else (+lim if x > +lim else x)
