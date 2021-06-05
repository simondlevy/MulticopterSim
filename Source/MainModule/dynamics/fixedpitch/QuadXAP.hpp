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

        virtual double computeRoll(double * omegas2)
        {
            return (omegas2[1] + omegas2[2]) - (omegas2[0] + omegas2[3]);
        }

        virtual double computePitch(double * omegas2)
        {
            return (omegas2[1] + omegas2[3]) - (omegas2[0] + omegas2[2]);
        }

        virtual int8_t getRotorDirection(uint8_t i) override
        {
            const int8_t dir[4] = {-1, -1, +1, +1};
            return dir[i];
        }

}; // class QuadXAP
