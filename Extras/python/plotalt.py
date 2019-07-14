#!/usr/bin/env python3
'''
Plots results from CSV log file

Copyright (C) 2019 Simon D. Levy

MIT License
'''

import numpy as np
import matplotlib.pyplot as plt
from sys import argv, stdin

infile = open(argv[1], 'r') if len(argv) > 1 else stdin

data = np.genfromtxt(infile, delimiter=',')

t = data[:,0]
z = data[:,1]

plt.plot(t, z)
plt.xlabel('time (sec)')
plt.ylabel('altitude (m)')
plt.ylim([min(z)-1, max(z)+1])
plt.show()
