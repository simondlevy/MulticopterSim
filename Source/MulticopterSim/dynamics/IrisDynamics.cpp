/*
 * Implemenation of IrisDynamics class
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#include "IrisDynamics.h"

// Get forces based on current motor values.
// See: https://charlestytler.com/modeling-vehicle-dynamics-6dof-nonlinear-simulation/#Equations-of-Motion
// XXX In reality, we get vertical thrust and angular velocities).
void IrisDynamics::getForces(double & Fz, double & L, double & M, double & N)
{
    // Convert motor values in [0,1] to thrusts in Newtons
    double F1 = Fthrust(_motorvals[0]);
    double F2 = Fthrust(_motorvals[1]);
    double F3 = Fthrust(_motorvals[2]);
    double F4 = Fthrust(_motorvals[3]);

    // Convert motor thrusts to angular accelerations
    L = (F2 + F3) - (F1 + F4);
    M = (F1 + F3) - (F2 + F4); 
    N = (T(F1) + T(F2)) - (T(F3) + T(F4)); 

    // Compute orthogonal force component Fz
    Fz = F1 + F2 + F3 + F4;
}

void IrisDynamics::setMotors(double * motorvals)
{
    for (uint8_t k=0; k<4; ++k) {
        _motorvals[k] = motorvals[k];
    }
}

// Factory method
MultirotorDynamics * MultirotorDynamics::create(void)
{
    return new IrisDynamics();
}
