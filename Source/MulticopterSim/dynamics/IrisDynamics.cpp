/*
 * Implemenation of IrisDynamics class
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#include "IrisDynamics.h"

// Get forces based on current motor values.
// XXX In reality, we get vertical thrust and angular velocities).
void IrisDynamics::getForces(double & Fz, double & L, double & M, double & N)
{
    double F1 = computeMotorThrust(_motorvals[0]);
    double F2 = computeMotorThrust(_motorvals[1]);
    double F3 = computeMotorThrust(_motorvals[2]);
    double F4 = computeMotorThrust(_motorvals[3]);

    // Convert motor values to rotational velocity vector
    L = motorsToAngularVelocity(1, 2, 0, 3);
    M = motorsToAngularVelocity(0, 2, 1, 3);
    N = (F1 + F2) - (F3 + F4); 

    // Compute orthogonal force component Fz
    Fz = F1 + F2 + F3 + F4;
}

void IrisDynamics::setMotors(double * motorvals)
{
    for (uint8_t k=0; k<4; ++k) {
        _motorvals[k] = motorvals[k];
    }
}

double IrisDynamics::motorsToAngularVelocity(uint8_t a, uint8_t b, uint8_t c, uint8_t d)
{
    double v = ((_motorvals[a] + _motorvals[b]) - (_motorvals[c] + _motorvals[d]));

    return (v < 0 ? -1 : +1) * fabs(v);
}

// Factory method
MultirotorDynamics * MultirotorDynamics::create(void)
{
    return new IrisDynamics();
}
