import nengo
import numpy as np
from multicopter_sim import Multicopter

model = nengo.Network()
with model:

    x = nengo.Ensemble(n_neurons=200, dimensions=2)

    synapse = 0.1
    def oscillator(x):
        r = 1
        s = 6
        return [synapse * (-x[1] * s + x[0] * (r - x[0]**2 - x[1]**2)) + x[0],
                synapse * ( x[0] * s + x[1] * (r - x[0]**2 - x[1]**2)) + x[1]]

    nengo.Connection(x, x, synapse=synapse, function=oscillator)
    
def on_step(sim):
    
    if not hasattr(on_step, 'copter'):
        on_step.copter = Multicopter()
        on_step.copter.start()
        
    if on_step.copter.isReady():
        print(on_step.copter.getState())
        on_step.copter.setMotors(.5*np.ones(4))
