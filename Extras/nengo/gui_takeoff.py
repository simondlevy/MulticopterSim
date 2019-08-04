import nengo
import numpy as np

from multicopter_sim import Multicopter
from nengo_pid_controller import NengoPidController

# Target 
ALTITUDE_TARGET = 10

# PID params
KP = 0.4
KD = 10.0
KI = 0.03

'''
# Create PID controller
pid = NengoPidController(KP, KD, KI)
model = nengo.Network()
pid.build(model)
with model:
    pass
'''

model = nengo.Network()

with model:

    pid = NengoPidController(model, KP, KD, KI)

    x = nengo.Ensemble(n_neurons=200, dimensions=2)

    synapse = 0.1
    def oscillator(x):
        r = 1
        s = 6
        
        x1 = synapse * (-x[1] * s + x[0] * (r - x[0]**2 - x[1]**2)) + x[0]
        x2 = synapse * ( x[0] * s + x[1] * (r - x[0]**2 - x[1]**2)) + x[1]
        
        return [x1, x2]

    nengo.Connection(x, x, synapse=synapse, function=oscillator)
    
def on_step(sim):
    
    if not hasattr(on_step, 'copter'):
        on_step.copter = Multicopter()
        on_step.copter.start()
        
    if on_step.copter.isReady():
        print(on_step.copter.getState())
        on_step.copter.setMotors(.5*np.ones(4))
