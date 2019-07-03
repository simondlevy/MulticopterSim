#!/usr/bin/env python
'''
  Altitude-hold example using Multicopter class

  Copyright(C) 2019 Simon D.Levy

  MIT License
'''

from time import sleep
from multicopter import Multicopter
import numpy as np

np.set_printoptions(precision=8)

copter = Multicopter()

copter.start()

for motorval in np.linspace(0, 1, 11):

    motorvals = motorval * np.ones(4)

    copter.setMotors(motorvals)

    telem = copter.getState()

    print('t: %5.0f  z: %+3.3f' % (telem[0], telem[10]))

    sleep(1)

copter.stop()
