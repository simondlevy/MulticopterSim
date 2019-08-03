import nengo
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

model = nengo.Network(seed=3)
with model:
    
    x = nengo.Ensemble(n_neurons=1200, dimensions=3, radius=30)

    synapse = 0.1

    def lorenz(x):

        sigma = 10
        beta = 8.0/3
        rho = 28
        
        dx0 = -sigma * x[0] + sigma * x[1]
        dx1 = -x[0] * x[2] - x[1]
        dx2 = x[0] * x[1] - beta * (x[2] + rho) - rho

        return [dx0 * synapse + x[0],
                dx1 * synapse + x[1],
                dx2 * synapse + x[2]]

    nengo.Connection(x, x, synapse=synapse, function=lorenz)
