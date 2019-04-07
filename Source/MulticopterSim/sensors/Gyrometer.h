/*
 * Gyrometer.h: Platform-indpendent class for emulating IMU gyrometer
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "NoisySensor.h"

class Gyrometer : public NoisySensor {

    public:

        Gyrometer(void);

        void computeAngularVelocity(double angularVelocity[3], double imuAngularVelocityRPY[3]);
};
