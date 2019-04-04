/*
 * Accelerometer.cpp: Emulate IMU accelerometer
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#include "Accelerometer.h"

#include <math.h>

Accelerometer::Accelerometer(double g)
{
    _g = g;
}

void Accelerometer::computeImuAcceleration(
        double verticalAcceleration,
        double rotation[3], 
        double imuLinearAccelerationXYZ[3])
{
    // Subtract vertical acceleration from earth gravity to simulate accelerometer
    double wdot = _g - verticalAcceleration;

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
}
