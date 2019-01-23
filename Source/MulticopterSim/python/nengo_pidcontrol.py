#!/usr/bin/env python3
'''
Nengo PID control

Copyright 2018 Terry Stewart, Simon D. Levy, Melanie Jouaiti

MIT License
'''

import nengo

class PIDController(object):

    def __init__(self, Kp, Kd, Ki=0, sim_time=.01, n_neurons=500, integral_synapse=0.1, integral_radius=1, seed=None):

        self.q_value = 0
        self.q_target_value = 0
        self.dq_value = 0
        self.last_actual = 0
        self.output_value = 0

        self.model = nengo.Network(seed=seed)

        self.Kp = Kp
        self.Kd = Kd
        self.sim_time = sim_time
        self.n_neurons = n_neurons

        with self.model:

            q_target = nengo.Node(lambda t: self.q_target_value, label='q_target')
            q = nengo.Node(lambda t: self.q_value, label='q')
            dq = nengo.Node(lambda t: self.dq_value, label='dq')
            
            dq_target = nengo.Node(None, size_in=1, label='dq_target')
            nengo.Connection(q_target, dq_target, synapse=None, transform=1) # direct feed-forward synapse
            nengo.Connection(q_target, dq_target, synapse=0, transform=-1)   # minimal time-step delay
            
            q_err = nengo.Ensemble(n_neurons=self.n_neurons, dimensions=1, label='q_err')
            nengo.Connection(q_target, q_err, synapse=None)
            nengo.Connection(q, q_err, synapse=None, transform=-1)

            q_err_integral = nengo.Ensemble(n_neurons=self.n_neurons, dimensions=1, radius=integral_radius,
                    label='q_err_integral')
            nengo.Connection(q_err, q_err_integral, synapse=integral_synapse, transform=integral_synapse)
            nengo.Connection(q_err_integral, q_err_integral, synapse=integral_synapse, transform=1)
            
            u = nengo.Node(None, size_in=1, label='u')    # output
            
            nengo.Connection(q_err, u, transform=self.Kp, synapse=None)

            nengo.Connection(q_err_integral, u, transform=Ki, synapse=None)
            
            dq_err = nengo.Ensemble(n_neurons=self.n_neurons, dimensions=1, label='dq_err')
            nengo.Connection(dq_target, dq_err, synapse=None)
            nengo.Connection(dq, dq_err, synapse=None, transform=-1)
            
            nengo.Connection(dq_err, u, transform=self.Kd, synapse=None)
            
            def output_func(t, x):
                self.output_value = x

            output = nengo.Node(output_func, size_in=1, label='output')
            nengo.Connection(u, output)

        self.sim = None

    def start(self):

        self.sim = nengo.Simulator(self.model, progress_bar=False)

    def getCorrection(self, target, actual):

        self.q_value = actual
        self.q_target_value = target
        self.dq_value = actual-self.last_actual

        self.last_actual = actual
       
        self.sim.run(self.sim_time)

        # Return the correction
        return self.output_value[0]

if __name__ == '__main__':

    p = PIDController(1, 1)

    for k in range(10):
         
        print(p.getCorrection(100, 0))
