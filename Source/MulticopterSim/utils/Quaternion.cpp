/*
 * Quaternion.cpp: quaternion from Euler angles
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#include "Quaternion.h"

#include <math.h>

Quaternion::Quaternion(void)
{
}

// https://graphics.fandom.com/wiki/Conversion_between_quaternions_and_Euler_angles
double * Quaternion::computeQuaternion(double eulerAngles[3])
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
    _q[0] = cph * cth * cps + sph * sth * sps;
    _q[1] = sph * cth * cps - cph * sth * sps;
    _q[2] = cph * sth * cps + sph * cth * sps;
    _q[3] = cph * cth * sps - sph * sth * cps;

    return _q;
}
