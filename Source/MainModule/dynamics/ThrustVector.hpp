/*
 * Dynamics class for thrust vectoring
 *
 * Copyright (C) 2020 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "../Dynamics.hpp"

class ThrustVectorDynamics : public Dynamics {

    public:	

        ThrustVectorDynamics(Parameters * params) 
            : Dynamics(4)
        {
            _p = params;

            _omegas = new double[4]();
            _omegas2 = new double[4]();
        }

        /**
         * Uses motor values to implement Equation 6.
         *
         * @param motorvals in interval [0,1]
         * @param dt time constant in seconds
         */
        virtual void setMotors(double* motorvals, double dt) override
        {
            // Convert the  motor values to radians per second
            for (unsigned int i = 0; i < 2; ++i) {
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

            debugline("%f  %f", motorvals[2], motorvals[3]);

            // Use the squared Omegas to implement the rest of Eqn. 6
            _U2 = 0;//_p->l * _p->b * u2(_omegas2);
            _U3 = 0;//_p->l * _p->b * u3(_omegas2);
            _U4 = 0;//_p->d * u4(_omegas2);
        }

    protected:

        // Dynamics method overrides

        // roll right
        virtual double u2(double * o) override
        {
            return (o[1] + o[2]) - (o[0] + o[3]);
        }

        // pitch forward
        virtual double u3(double * o) override
        {
            return (o[1] + o[3]) - (o[0] + o[2]);
        }

        // yaw cw
        virtual double u4(double * o) override
        {
            return (o[0] + o[1]) - (o[2] + o[3]);
        }

        // motor direction for animation
        virtual int8_t motorDirection(uint8_t i) override
        {
            const int8_t dir[2] = {+1, -1};
            return dir[i];
        }

}; // class ThrustVector
