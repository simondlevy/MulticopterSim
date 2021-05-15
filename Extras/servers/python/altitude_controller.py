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

    def u(self, z, dzdt, dt):

        # Compute dzdt setpoint and error
        dzdt_target = (self.target - z) * self.Kp_pos
        dzdt_error = dzdt_target - dzdt

        # Update error integral and error derivative
        self.integralError = AltitudeController._constrainAbs(
                        self.integralError + dzdt_error * dt, self.windupMax)

        # Compute control u
        return self.Kp_vel * dzdt_error + self.Ki_vel * self.integralError

    def _constrainAbs(x, lim):

        return -lim if x < -lim else (+lim if x > +lim else x)
