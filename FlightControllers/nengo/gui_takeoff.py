'''
   Test simple altitude-hold PID controller in Nengo GUI

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

from altitude_hold import buildpid, runpid, startcopter

model, pid = buildpid()

with model:

    pass


def on_step(sim):

    if not hasattr(on_step, 'copter'):

        on_step.copter = startcopter()

    if on_step.copter.isReady():

        runpid(on_step.copter, pid)
