/*
* Dynamics class for multirotors
*
* Copyright (C) 2020 Simon D. Levy
*
* MIT License
*/

#pragma once

#include "../Dynamics.hpp"

class MultirotorDynamics : public Dynamics {

    public:

        MultirotorDynamics(Parameters * params, const uint8_t motorCount) 
            : Dynamics(motorCount)
        {
            _p = params;

            _omegas = new double[motorCount]();
            _omegas2 = new double[motorCount]();
        }

        /**
         * Uses motor values to implement Equation 6.
         *
         * @param motorvals in interval [0,1]
         * @param dt time constant in seconds
         */
        virtual void setMotors(double* motorvals, double dt) override
        {
            debugline("%+3.3f  %+3.3f  %+3.3f  %+3.3f", motorvals[0], motorvals[1], motorvals[2], motorvals[3]);

            // Convert the  motor values to radians per second
            for (unsigned int i = 0; i < _motorCount; ++i) {
                _omegas[i] = computeMotorSpeed(motorvals[i]); //rad/s
            }

            // Compute overall torque from omegas before squaring
            _Omega = u4(_omegas);

            // Overall thrust is sum of squared omegas
            _U1 = 0;
            for (unsigned int i = 0; i < _motorCount; ++i) {
                _omegas2[i] = _omegas[i] * _omegas[i];
                _U1 += _p->b * _omegas2[i];
            }

            // Use the squared Omegas to implement the rest of Eqn. 6
            _U2 = _p->l * _p->b * u2(_omegas2);
            _U3 = _p->l * _p->b * u3(_omegas2);
            _U4 = _p->d * u4(_omegas2);
        }

 }; // class MultirotorDynamics
