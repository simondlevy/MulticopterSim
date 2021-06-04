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
            u4 = o[0] - o[1];
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

        virtual void setMotors(double* motorvals) override
        {
            static constexpr double FAKE_B = 5.E-06;
            static constexpr double FAKE_L = 3.5;

            Dynamics::setMotors(motorvals);

            // Overall thrust U1 is sum of squared omegas
            _U1 = 0;
            for (unsigned int i = 0; i < _rotorCount; ++i) {
                _omegas2[i] = _wparams.rho * _omegas[i] * _omegas[i];
                _U1 += FAKE_B * _omegas2[i];
            }

            // Torque forces are computed differently for each vehicle configuration
            double u2=0, u3=0, u4=0;
            computeTorques(motorvals, u2, u3, u4);

            _U2 = FAKE_L * FAKE_B * u2;
            _U3 = FAKE_L * FAKE_B * u3;
            _U4 = FAKE_B * u4;
        }

}; // class CoaxialDynamics
