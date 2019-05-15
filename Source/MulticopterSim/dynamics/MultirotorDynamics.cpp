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

        _acceleration[j] = 0;
        _velocity[j] = 0;
    }

    _airborne = airborne;
}

void MultirotorDynamics::update(
        double dt, 
        double motorvals[4],
        double imuAngularVelocityRPY[3], 
        double eulerAngles[3], 
        double velocityXYZ[3],
        double positionXYZ[3])
{
    // Forces
    double Fz = 0;
    double L  = 0;
    double M  = 0;
    double N  = 0;
    
    // Convert motor values to forces (in reality, we get vertical thrust and angular velocities)
    motorsToForces(motorvals, Fz, L, M, N);

    // XXX For now, we go directly from rotational force to angular velocity
    imuAngularVelocityRPY[0] = L;
    imuAngularVelocityRPY[1] = M;
    imuAngularVelocityRPY[2] = N;

    // Integrate rotational velocity to get Euler angles
    for (uint8_t j=0; j<3; ++j) {
        _rotation[j] += dt * imuAngularVelocityRPY[j];
        eulerAngles[j] = _rotation[j];
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
    // http://www.chrobotics.com/library/understanding-euler-angles
    // 
    // We negate X and Y to adjust for the fact that roll right and pitch forward correspond to 
    // negative Euler angles.
    _acceleration[0] = -Fz * (sphi*spsi + cphi*cpsi*sthe);
    _acceleration[1] = -Fz * (cphi*spsi*sthe - cpsi*sphi);
    _acceleration[2] =  Fz * (cphi*cthe);

    // XXX Subtract off an empirical constant to get net acceleration vertical, so that motionless maps to zero
    _acceleration[2] -= 2.01;

    // We're airborne once net vertical acceleration excedes zero
    if (!_airborne) {
        _airborne = _acceleration[2] > 0;
    }

    // Once airborne, we can compute inertial-frame state values
    if (_airborne) {

        for (uint8_t j=0; j<3; ++j) {

            // Integrate acceleration to get velocity
            _velocity[j] += dt * _acceleration[j];

            // Integrate velocity to get position
            _position[j] += dt * _velocity[j];

            // Copy out values
            velocityXYZ[j] = _velocity[j];
            positionXYZ[j] = _position[j];
        }
    }
}

// Convert motor values to forces (in reality, we get vertical thrust and angular velocities)
void MultirotorDynamics::motorsToForces(double * motorvals, double & Fz, double & L, double & M, double & N)
{
    // Convert motor values to rotational velocity vector
    L = motorsToAngularVelocity(motorvals, 1, 2, 0, 3);
    M = motorsToAngularVelocity(motorvals, 0, 2, 1, 3);
    N = motorsToAngularVelocity(motorvals, 0, 1, 2, 3);

    // Compute orthogonal force vector
    Fz = motorvals[0] + motorvals[1] + motorvals[2] + motorvals[3];
}

double MultirotorDynamics::motorsToAngularVelocity(double motorvals[4], uint8_t a, uint8_t b, uint8_t c, uint8_t d)
{
    double v = ((motorvals[a] + motorvals[b]) - (motorvals[c] + motorvals[d]));

    return (v < 0 ? -1 : +1) * fabs(v);
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

