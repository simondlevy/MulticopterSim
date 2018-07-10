'''
Nengo-based PI controller

Copyright 2018 Terry Stewart, Simon D. Levy

MIT License
'''
import nengo

class PIController(object):

    def __init__(self, varioP, varioI, sim_time=.01, n_neurons=200, 
            integral_synapse=0.1, integral_radius=1, seed=None, in_gui=False):

        self.Kp = varioP
        self.Ki = varioI

        self.inBandPrev = False

        self.STICK_DEADBAND = 0.15

        self.q_value = 0
        self.q_target_value = 0
        self.output_value = 0

        self.model = nengo.Network(seed=seed)

        self.sim_time = sim_time
        self.n_neurons = n_neurons

        with self.model:

            q_target = nengo.Node(lambda t: self.q_target_value, label='q_target')
            q = nengo.Node(lambda t: self.q_value, label='q')
            
            q_err = nengo.Ensemble(n_neurons=self.n_neurons, dimensions=1, label='q_err')
            nengo.Connection(q_target, q_err, synapse=None)
            nengo.Connection(q, q_err, synapse=None, transform=-1)

            q_err_integral = nengo.Ensemble(n_neurons=self.n_neurons, dimensions=1, radius=integral_radius, label='q_err_integral')
            nengo.Connection(q_err, q_err_integral, synapse=integral_synapse, transform=integral_synapse)
            nengo.Connection(q_err_integral, q_err_integral, synapse=integral_synapse, transform=1)
            
            u = nengo.Node(None, size_in=1, label='u')    # output
            
            nengo.Connection(q_err, u, transform=self.Kp, synapse=None)

            nengo.Connection(q_err_integral, u, transform=self.Ki, synapse=None)
            
            def output_func(t, x):
                self.output_value = x

            output = nengo.Node(output_func, size_in=1, label='output')
            nengo.Connection(u, output)
        
        self.in_gui = in_gui
        self.sim = None

        self.inBandPrev = False

    def getCorrection(self, throttle, variometer):

        # Start Nengo simulator if it isn't alredy running
        if self.sim is None:  
            self.sim = nengo.Simulator(self.model, progress_bar=False)  

        # Target is zero velocity
        self.q_value = variometer
        self.q_target_value = 0

        self.sim.run(self.sim_time)

        # Reset integral if moved into stick deadband
        inBandCurr = abs(throttle) < self.STICK_DEADBAND
        if inBandCurr and not self.inBandPrev:
            self.sim.reset()
        self.inBandPrev = inBandCurr

        return self.output_value[0]
