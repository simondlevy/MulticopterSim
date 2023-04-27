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
#include "../../mixers/quadxap.h"

class QuadXAPDynamics : public FixedPitchDynamics {

    public:	

        QuadXAPDynamics(
                Dynamics::vehicle_params_t &vparams,
                FixedPitchDynamics::fixed_pitch_params_t &fparams,
                bool autoland=true)
            : FixedPitchDynamics(4, vparams, fparams, mixerQuadXAP, autoland)
        {
        }

}; // class QuadXAP
