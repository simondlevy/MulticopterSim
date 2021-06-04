/*
 * Dynamics class for thrust vectoring
 *
 * Copyright (C) 2020 Simon D. Levy, Noah Ghosh
 *
 * MIT License
 */

#pragma once

#include "../Dynamics.hpp"
#define _USE_MATH_DEFINES
#include <math.h>

class ThrustVectorDynamics : public Dynamics {

    private:

        // radians
        double _nozzleMaxAngle = 0;

    protected:

        // Dynamics method overrides

        virtual void computeTorques(double * motorvals, double & u2, double & u3, double & u4) override
        {
            // shorthand
            double * o = _omegas2;

            // thrust in direction of barrel is sum of rotor rotations
            double thrust = o[0] + o[1];

            // roll right is thrust time sine of nozzle angle along right/left axis
            u2 = thrust * sin(motorvals[2] * _nozzleMaxAngle);

            // pitch forward is thrust time sine of nozzle angle along forward/backward axis
            u3 = thrust * sin(motorvals[3] * _nozzleMaxAngle);

            // yaw clockwise is difference between rotor rotations
            u4 = (o[0] - o[1]);
        }

        // motor direction for animation
        virtual int8_t rotorDirection(uint8_t i) override
        {
            const int8_t dir[2] = {-1, +1};
            return dir[i];
        }

    public:	

        ThrustVectorDynamics(Dynamics::vehicle_params_t &vparams, double nozzleMaxAngle)
            : Dynamics(4, vparams)
        {
            _rotorCount = 2;

            // degrees => radians
            _nozzleMaxAngle = M_PI * nozzleMaxAngle / 180;
        }

        virtual void setMotors(double* motorvals) override
        {
            Dynamics::setMotors(motorvals);

            // Overall thrust U1 is sum of squared omegas
            _U1 = 0;
            for (unsigned int i = 0; i < _rotorCount; ++i) {
                _omegas2[i] = _wparams.rho * _omegas[i] * _omegas[i];
                _U1 += _omegas2[i];
            }

            // Torque forces are computed differently for each vehicle configuration
            double u2=0, u3=0, u4=0;
            computeTorques(motorvals, u2, u3, u4);

            _U2 = u2;
            _U3 = u3;
            _U4 = u4;
        }

}; // class ThrustVectorDynamics
