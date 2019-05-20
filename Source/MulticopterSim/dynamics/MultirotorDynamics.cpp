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
    for (uint8_t j=0; j<3; ++j) {

        _position[j] = position[j];
        _rotation[j] = rotation[j];
        _angularVelocity[j] = 0;
        _velocity[j] = 0;
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
    _angularVelocity[0] = L;
    _angularVelocity[1] = M;
    _angularVelocity[2] = N;

    // Integrate rotational velocity to get Euler angles
    for (uint8_t j=0; j<3; ++j) {
        _rotation[j] += dt * _angularVelocity[j];
    }

    // Rename Euler angles for readability
    double phi   = _rotation[0];
    double theta = _rotation[1];
    double psi   = _rotation[2];

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
            _velocity[j] += dt * accel[j];

            // Integrate velocity to get position
            _position[j] += dt * _velocity[j];
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
        angularVelocity[j] = _angularVelocity[j];
        eulerAngles[j] = _rotation[j];
        velocityXYZ[j] = _velocity[j];
        positionXYZ[j] = _position[j];
    }
}

void MultirotorDynamics::eulerToQuaternion(double eulerAngles[3], double quaternion[4])
{
    // Convenient renaming
    double phi = eulerAngles[0] / 2;
    double the = eulerAngles[1] / 2;
    double psi = eulerAngles[2] / 2;

    // Pre-computation
    double cph = cos(phi);
    double cth = cos(the);
    double cps = cos(psi);
    double sph = sin(phi);
    double sth = sin(the);
    double sps = sin(psi);

    // Conversion
    quaternion[0] =  cph * cth * cps + sph * sth * sps;
    quaternion[1] =  cph * sth * sps - sph * cth * cps ;
    quaternion[2] = -cph * sth * cps - sph * cth * sps;
    quaternion[3] =  cph * cth * sps - sph * sth * cps;
}
