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

        
        static constexpr double FAKE_COLLECTIVE = 5.E-06; // XXX

        static constexpr double CYCLIC_COEFFICIENT = 1.0;

        static double computeCyclic(double * motorvals, uint8_t axis)
        {
            return CYCLIC_COEFFICIENT * motorvals[axis];
        }

    protected:

        virtual double getThrustCoefficient(double * motorvals) override
        {
            (void)motorvals;
            return FAKE_COLLECTIVE;
        }

        virtual void computeRollAndPitch(double * motorvals, double * omegas2, double & roll, double & pitch) override
        {
            // For a coaxial, rotor speeds do not determine roll and pitch
            (void)omegas2;
            
            roll = computeCyclic(motorvals, 2);
            pitch = computeCyclic(motorvals, 3);
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
