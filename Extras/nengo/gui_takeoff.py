'''
Test simple altitude-hold PID controller in Nengo GUI

Copyright (C) 2019 Simon D. Levy

MIT License
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


