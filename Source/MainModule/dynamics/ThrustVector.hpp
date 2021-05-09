/*
 * Dynamics class for thrust vectoring
 *
 * Copyright (C) 2020 Simon D. Levy, Noah Ghosh
 *
 * MIT License
 */

#pragma once

#include "../Dynamics.hpp"

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
            u4 = (o[1] - o[0]);
        }

        // motor direction for animation
        virtual int8_t rotorDirection(uint8_t i) override
        {
            const int8_t dir[2] = {+1, -1};
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

}; // class ThrustVector
