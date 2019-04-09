/*
 * Accelerometer.h: Platform-indpendent class for emulating IMU accelerometer 
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "NoisySensor.h"

class Accelerometer : public NoisySensor {

    private:

        double _xyz[3];

    public:

        Accelerometer(void);

        double * computeImuAcceleration(double verticalAcceleration, double rotation[3], double G = 9.80665);
};
