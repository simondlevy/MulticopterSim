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

    public:	

        ThrustVectorDynamics(
                const double b,
                const double d,
                const double m,
                const double Ix,
                const double Iy,
                const double Iz,
                const double Jr,
                uint16_t maxrpm,
                const double barrelHeight,
                const double nozzleOffset)
            : Dynamics(4, b, d, m, Ix, Iy, Iz, Jr, barrelHeight/2-nozzleOffset, maxrpm)
        {
            _rotorCount = 2;
        }

    protected:

        // Dynamics method overrides

        virtual void computeTorques(double * motorvals, double & u2, double & u3, double & u4) override
        {
            // shorthand
            double * o = _omegas2;

            // roll right
            u2 = 0;

            // pitch forward
            u3 = 0;

            // yaw clockwise
            u4 = (o[0] - o[1]);
        }

        // motor direction for animation
        virtual int8_t rotorDirection(uint8_t i) override
        {
            const int8_t dir[2] = {+1, -1};
            return dir[i];
        }

}; // class ThrustVector
