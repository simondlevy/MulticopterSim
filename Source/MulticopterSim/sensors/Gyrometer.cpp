/*
 * Gyrometer.cpp: Emulate IMU gyrometer
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#include "Gyrometer.h"

// 3 dimensions, noise mean = 0 , noise stdev = 0.0001
Gyrometer::Gyrometer(void) : NoisySensor(3, 0, 0.0001)
{
}

void Gyrometer::computeAngularVelocity(double angularVelocity[3], double imuAngularVelocityRPY[3])
{
    for (uint8_t k=0; k<3; ++k) {
        imuAngularVelocityRPY[k] = angularVelocity[k];
    }
    
    addNoise(imuAngularVelocityRPY);
}
