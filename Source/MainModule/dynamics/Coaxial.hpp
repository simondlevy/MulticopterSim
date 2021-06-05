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

    private:

        // XXX
        static constexpr double FAKE_B = 5.E-06;
        static constexpr double FAKE_L = 3.5;

    protected:

        virtual double getThrustCoefficient(double * motorvals) override
        {
            (void)motorvals;
            return FAKE_B;
        }

        virtual double computeRoll(double * motorvals, double * omegas2) override
        {
            // For a coaxial, rotor speeds do not determine roll and pitch
            (void)omegas2;

            return FAKE_B * FAKE_L * motorvals[2];
         }

        virtual double computePitch(double * motorvals, double * omegas2) override
        {
            // For a coaxial, rotor speeds do not determine roll and pitch
            (void)omegas2;
            
            return FAKE_B * FAKE_L * motorvals[3];
         }

        // motor direction for animation
        virtual int8_t getRotorDirection(uint8_t i) override
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

}; // class CoaxialDynamics
