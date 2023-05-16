/*
 * Dynamics class for quad-X frames using Betaflight motor layout:
 *
 *    4cw   2ccw
 *       \ /
 *        ^
 *       / \
 *    3ccw  1cw
 *
 * Copyright (C) 2022 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "../Axes.hpp"
#include "../FixedPitch.hpp"

class QuadXBFDynamics : public FixedPitchDynamics {

    private:

        axes_t mixer[4] = {
            //  rol   pit    yaw
            { -1.0f, +1.0f, -1.0f },          // REAR_R
            { -1.0f, -1.0f, +1.0f },          // FRONT_R
            { +1.0f, +1.0f, +1.0f },          // REAR_L
            { +1.0f, -1.0f, -1.0f },          // FRONT_L
        };

    public:	

        QuadXBFDynamics(
                Dynamics::vehicle_params_t &vparams,
                FixedPitchDynamics::fixed_pitch_params_t &fparams,
                bool autoland=true)
            : FixedPitchDynamics(4, vparams, fparams, mixer, autoland)
        {
        }

}; // class QuadXBF
