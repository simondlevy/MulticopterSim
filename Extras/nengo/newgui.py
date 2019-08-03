import nengo
import numpy as np
from multicopter_sim import Multicopter

# Target 
ALTITUDE_TARGET = 10

# PID params
KP = 0.4
KD = 10.0
KI = 0.03

# Create a multicopter and start communications with simulator
copter = Multicopter()
copter.start()

q_value = 0
q_target_value = 0
dq_value = 0
last_actual = 0
output_value = 0

model = nengo.Network(seed=3)

with model:
    
    q_target = nengo.Node(lambda t: q_target_value, label='q_target')
    q = nengo.Node(lambda t: q_value, label='q')
    dq = nengo.Node(lambda t: dq_value, label='dq')

    x = nengo.Ensemble(n_neurons=1200, dimensions=3, radius=30)

    synapse = 0.1

    def lorenz(x):

        sigma = 10
        beta = 8.0/3
        rho = 28
        
        dx0 = -sigma * x[0] + sigma * x[1]
        dx1 = -x[0] * x[2] - x[1]
        dx2 = x[0] * x[1] - beta * (x[2] + rho) - rho

        # Get vehicle state from sim
        telem = copter.getState()

        # Extract time from state
        t =  telem[0]

        # Extract altitude from state.  Altitude is in NED coordinates, so we negate it to use as input
        # to PID controller.
        z = -telem[9]

        print(z)

        u = 0.5

        # Set motor values in sim
        copter.setMotors(u*np.ones(4))

        return [dx0 * synapse + x[0],
                dx1 * synapse + x[1],
                dx2 * synapse + x[2]]

    nengo.Connection(x, x, synapse=synapse, function=lorenz)
