'''
   Mixers for Python-based control

   Copyright (C) 2021 Simon D. Levy

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

import numpy as np


class Mixer:

    def __init__(self, d):

        self.d = d

    def getMotors(self, u):
        '''
        Converts demands U [throttle, roll, pitch, yaw] into four motor demands
        Omega
        '''

        omega = np.zeros(4)

        for i in range(4):
            omega[i] = (u[0] * self.d[i][0] + u[1] * self.d[i][1] +
                        u[2] * self.d[i][2] + u[3] * self.d[i][3])

        return omega


class PhantomMixer(Mixer):

    def __init__(self):

        Mixer.__init__(self,
                       # Th  RR  PF  YR
                       ((+1, -1, -1, +1),   # 1 right front
                        (+1, +1, +1, +1),   # 2 left rear
                        (+1, +1, -1, -1),   # 3 left front
                        (+1, -1, +1, -1)))  # 4 right rear


class IngenuityMixer(Mixer):

    def __init__(self):

        Mixer.__init__(self,
                       # Th  RR  PF  YR
                       ((+1,  0,  0, +1),   # 1 right front
                        (+1,  0,  0, -1),   # 2 left rear
                        (0,  +1,  0,  0),   # 3 left front
                        (0,   0, +1,  0)))  # 4 right rear
