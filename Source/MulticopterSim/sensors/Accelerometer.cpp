/*
 * Accelerometer.cpp: Emulate IMU accelerometer
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#include "Accelerometer.h"

#include <math.h>

Accelerometer::Accelerometer(void) : NoisySensor(3, 0, 0)
{
}

void Accelerometer::computeImuAcceleration(
        double verticalAcceleration,
        double rotation[3], 
        double imuLinearAccelerationXYZ[3],
        double G) 
{
    // Subtract earth gravity from vertical acceleration to simulate accelerometer
    double wdot = verticalAcceleration - G;

    // Store stuff in a readable format
    double phi   = rotation[0]; // bank angle
    double theta = rotation[1]; // pitch attitude

    // Pre-calculate trig values
    double cthe = cos(theta); 

    // Rotate vertical acceleration into vehicle frame and add earth gravity to get 
    // accelerometer reading
    imuLinearAccelerationXYZ[0] = wdot * -sin(theta);
    imuLinearAccelerationXYZ[1] = wdot * sin(phi) * cthe;
    imuLinearAccelerationXYZ[2] = wdot * cos(phi) * cthe;

    // Add noise
    addNoise(imuLinearAccelerationXYZ);
}
