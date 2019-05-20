/*
 * Implemenation of platform-independent multirotor dynamics
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#include "MultirotorDynamics.h"
#include "debug.h"    
#include <math.h>

void MultirotorDynamics::init(double position[3], double rotation[3], bool airborne)
{
    // Zero-out rates
    for (uint8_t j=0; j<6; ++j) {
        _x[j] = 0;
    }

    // Set pose
    for (uint8_t j=0; j<3; ++j) {

        _x[j+6] = rotation[j];
        _x[j+9] = position[j];
    }

    _airborne = airborne;
}

void MultirotorDynamics::update(double dt)
{
    // Forces
    double Fz = 0;
    double L  = 0;
    double M  = 0;
    double N  = 0;
    
    // Use frame subclass (e.g., Iris) to convert motor values to forces (in
    // reality, we get vertical thrust and angular velocities)
    getForces(Fz, L, M, N);

    // XXX For now, we go directly from rotational force to angular velocity
    //
    _x[3] = L;
    _x[4] = M;
    _x[5] = N;

    // Integrate rotational velocity to get Euler angles
    for (uint8_t j=0; j<3; ++j) {
        _x[j+6] += dt * _x[j+3];
    }

    // Rename Euler angles for readability
    double phi   = _x[6];
    double theta = _x[7];
    double psi   = _x[8];

    // Pre-compute angle trigonometry for rotation to earth frame
    double sphi = sin(phi);
    double spsi = sin(psi);
    double cphi = cos(phi);
    double cpsi = cos(psi);
    double sthe = sin(theta);
    double cthe = cos(theta);

    // Rotate orthongal force vecotor into intertial frame to compute translation force.  
    // See last column of rotation matrix at end of section 5 in
    //   http://www.chrobotics.com/library/understanding-euler-angles
    // Note use of negative sign to implement North-East-Down (NED) coordinates
    double accel[3] = { -Fz * (sphi*spsi + cphi*cpsi*sthe), -Fz * (cphi*spsi*sthe - cpsi*sphi), -Fz * (cphi*cthe) };

    // Add earth gravity to get net acceleration vertical, so that motionless maps to zero
    accel[2] += G;

    // We're airborne once net vertical acceleration falls below zero
    if (!_airborne) {
        _airborne = accel[2] < 0;
    }

    // Once airborne, we can compute inertial-frame state values by integration
    if (_airborne) {

        for (uint8_t j=0; j<3; ++j) {

            // Integrate acceleration to get velocity
            _x[j] += dt * accel[j];

            // Integrate velocity to get position
            _x[j+9] += dt * _x[j];
        }
    }
}

void MultirotorDynamics::getState(
        double angularVelocity[3], 
        double eulerAngles[3], 
        double velocityXYZ[3],
        double positionXYZ[3])
{
    for (uint8_t j=0; j<3; ++j) {
        angularVelocity[j] = _x[j+3];
        eulerAngles[j]     = _x[j+6];
        velocityXYZ[j]     = _x[j];
        positionXYZ[j]     = _x[j+9];
    }
}

double MultirotorDynamics::computeMotorThrust(double motorval)
{
    return 4 * motorval; // XXX should use nonlinear formula
}
