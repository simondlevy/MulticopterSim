/*
 * Dynamics class for quad-X frames using ArduPilot motor layout:
 *
 *    3cw   1ccw
 *       \ /
 *        ^
 *       / \
 *    2ccw  4cw
 *
 * Copyright (C) 2019 Simon D. Levy, Daniel Katzav
 *
 * MIT License
 */

#pragma once

#include "../FixedPitch.hpp"

class QuadXAPDynamics : public FixedPitchDynamics {

    public:	

        QuadXAPDynamics(Dynamics::vehicle_params_t &vparams, FixedPitchDynamics::fixed_pitch_params_t &fparams)
            : FixedPitchDynamics(4, vparams, fparams)
        {
        }

    protected:

        // FixedPitchDynamics method overrides

        virtual double u2(void) override
        {
            // shorthand
            double * o = _omegas2;

            return (o[1] + o[2]) - (o[0] + o[3]);
        }

        virtual double u3(void) override
        {
            // shorthand
            double * o = _omegas2;

            // pitch forward
            return (o[1] + o[3]) - (o[0] + o[2]);
        }

        virtual double u4(void) override
        {
            // shorthand
            double * o = _omegas2;

            return (o[0] + o[1]) - (o[2] + o[3]);
        }

        virtual double omega(void) override
        {
            // shorthand
            double * o = _omegas;

            return (o[0] + o[1]) - (o[2] + o[3]);
        }

        // rotor direction for animation
        virtual int8_t rotorDirection(uint8_t i) override
        {
            const int8_t dir[4] = {-1, -1, +1, +1};
            return dir[i];
        }

}; // class QuadXAP
