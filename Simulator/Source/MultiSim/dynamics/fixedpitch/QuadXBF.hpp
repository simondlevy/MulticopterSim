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
#include "../../mixers/quadxbf.h"

class QuadXBFDynamics : public FixedPitchDynamics {

    public:	

        QuadXBFDynamics(
                Dynamics::vehicle_params_t &vparams,
                FixedPitchDynamics::fixed_pitch_params_t &fparams,
                bool autoland=true)
            : FixedPitchDynamics(4, vparams, fparams, mixerQuadXBF, autoland)
        {
        }

}; // class QuadXBF
