/*
 * Implemenation of IrisDynamics class
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#include "MultirotorDynamicsIris.h"

// Convert motor values to forces (in reality, we get vertical thrust and angular velocities)
void IrisDynamics::motorsToForces(double * motorvals, double & Fz, double & L, double & M, double & N)
{
    // Convert motor values to rotational velocity vector
    L = motorsToAngularVelocity(motorvals, 1, 2, 0, 3);
    M = motorsToAngularVelocity(motorvals, 0, 2, 1, 3);
    N = motorsToAngularVelocity(motorvals, 0, 1, 2, 3);

    // Compute orthogonal force vector
    Fz = motorvals[0] + motorvals[1] + motorvals[2] + motorvals[3];
}

double IrisDynamics::motorsToAngularVelocity(double motorvals[4], uint8_t a, uint8_t b, uint8_t c, uint8_t d)
{
    double v = ((motorvals[a] + motorvals[b]) - (motorvals[c] + motorvals[d]));

    return (v < 0 ? -1 : +1) * fabs(v);
}

// Factory method
MultirotorDynamics * MultirotorDynamics::create(void)
{
    return new IrisDynamics();
}

