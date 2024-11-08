'''
   Simple altitude-hold and fly-forward

   Copyright (C) 2019 Simon D. Levy

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
