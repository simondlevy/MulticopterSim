
#include <mixers/quadxbf.h>
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

#include "../FixedPitch.hpp"

class QuadXBFDynamics : public FixedPitchDynamics {

    public:	

        QuadXBFDynamics(
                Dynamics::vehicle_params_t &vparams,
                FixedPitchDynamics::fixed_pitch_params_t &fparams)
            : FixedPitchDynamics(4, vparams, fparams, mixerQuadXBF)
        {
        }

}; // class QuadXBF
