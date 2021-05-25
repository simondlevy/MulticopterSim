/*
 * Dynamics class for coaxial copters
 *
 * Copyright (C) 2021 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "../Dynamics.hpp"

class CoaxialDynamics : public Dynamics {

    protected:

        // Dynamics method overrides

        virtual void computeTorques(double * motorvals, double & u2, double & u3, double & u4) override
        {
            // shorthand
            double * o = _omegas2;

            // thrust in direction of barrel is sum of rotor rotations
            double thrust = o[0] + o[1];

            // for now use raw servo values for roll, pitch
            u2 = thrust * motorvals[2];
            u3 = thrust * motorvals[3];

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

        CoaxialDynamics(Dynamics::vehicle_params_t &vparams)
            : Dynamics(4, vparams)
        {
            _rotorCount = 2;
        }

}; // class Coaxial
