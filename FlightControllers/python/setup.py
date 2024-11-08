'''
   Python distutils setup file for installing multicopter_sim

   Copyright(C) 2019 Simon D.Levy

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

from distutils.core import setup

setup(name='MulticopterSim',
      packages=['multicopter_server'],
      version='0.1',
      description='Talk to MulticopterSim over sockets',
      author_email='simon.d.levy@gmail.com',
      url='https://github.com/simondlevy/MulticopterSim/Extras/python',
      license='MIT',
      platforms='Linux; Windows')
