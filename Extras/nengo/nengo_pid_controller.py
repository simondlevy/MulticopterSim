#!/usr/bin/env python3
'''
Nengo PID control

Copyright 2018 Terry Stewart, Simon D. Levy, Melanie Jouaiti

MIT License
'''

import nengo
import numpy as np

from sys import stdout
from time import sleep

class NengoPidController(object):

    def __init__(self, Kp=0, Kd=0, Ki=0, n_dims=1, sim_time=.001, n_neurons=100, 
            integral_synapse=0.1, integral_radius=2, seed=None, in_gui=False):

        self.q_value = np.zeros(n_dims)
        self.q_target_value = np.zeros(n_dims)
        self.dq_value = np.zeros(n_dims)
        self.last_actual = np.zeros(n_dims)
        self.output_value = np.zeros(n_dims)

        self.model = nengo.Network(seed=seed)

        self.Kp = Kp
        self.Kd = Kd
        self.n_dims = n_dims
        self.sim_time = sim_time
        self.n_neurons = n_neurons

        with self.model:

            q_target = nengo.Node(lambda t: self.q_target_value, label='q_target')
            q = nengo.Node(lambda t: self.q_value, label='q')
            dq = nengo.Node(lambda t: self.dq_value, label='dq')
            
            dq_target = nengo.Node(None, size_in=self.n_dims, label='dq_target')
            nengo.Connection(q_target, dq_target, synapse=None, transform=1) # direct feed-forward synapse
            nengo.Connection(q_target, dq_target, synapse=0, transform=-1)   # minimal time-step delay
            
            q_err = nengo.Ensemble(n_neurons=self.n_neurons, dimensions=self.n_dims, label='q_err')
            nengo.Connection(q_target, q_err, synapse=None)
            nengo.Connection(q, q_err, synapse=None, transform=-1)

            q_err_integral = nengo.Ensemble(n_neurons=self.n_neurons, dimensions=self.n_dims, radius=integral_radius,
                    label='q_err_integral')
            nengo.Connection(q_err, q_err_integral, synapse=integral_synapse, transform=integral_synapse)
            nengo.Connection(q_err_integral, q_err_integral, synapse=integral_synapse, transform=1)
            
            u = nengo.Node(None, size_in=self.n_dims, label='u')    # output
            
            nengo.Connection(q_err, u, transform=self.Kp, synapse=None)

            nengo.Connection(q_err_integral, u, transform=Ki, synapse=None)
            
            dq_err = nengo.Ensemble(n_neurons=self.n_neurons, dimensions=self.n_dims, label='dq_err')
            nengo.Connection(dq_target, dq_err, synapse=None)
            nengo.Connection(dq, dq_err, synapse=None, transform=-1)
            
            nengo.Connection(dq_err, u, transform=self.Kd, synapse=None)
            
            def output_func(t, x):
                self.output_value = x

            output = nengo.Node(output_func, size_in=self.n_dims, label='output')
            nengo.Connection(u, output)


        if not in_gui:  
            self.sim = nengo.Simulator(self.model, progress_bar=False)  

    def getCorrection(self, target, actual):

        # Make inputs into arrays if they aren't already
        target = np.array(target)
        actual = np.array(actual)

        self.q_value = actual
        self.q_target_value = target
        self.dq_value = actual-self.last_actual

        self.last_actual = actual
       
        self.sim.run(self.sim_time)

        # Return correction as tuple
        return tuple(self.output_value)

if __name__ == '__main__':

    p = NengoPidController(1, 1)

    while True:
         
        print(p.getCorrection(100, 0))
