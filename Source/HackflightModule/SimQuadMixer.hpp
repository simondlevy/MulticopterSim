/*
* Helper class for quadcopter mixer
*
* Copyright (C) 2021 Simon D. Levy
*
* MIT License
*/

#pragma once

#include <mixers/quad/xap.hpp>

#include "SimMotor.hpp"

class SimQuadMixer {

    public:

        SimRotaryMotor motor1;
        SimRotaryMotor motor2;
        SimRotaryMotor motor3;
        SimRotaryMotor motor4;

        hf::MixerQuadXAP mixer = hf::MixerQuadXAP(&motor1, &motor2, &motor3, &motor4);

        SimMotor * motors[4] = {&motor1, &motor2, &motor3, &motor4};

}; // class SimQuadMixer
