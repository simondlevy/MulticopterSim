/*
 * Header-only code for coordinate transforms
 *
 * See Section 5 of http://www.chrobotics.com/library/understanding-euler-angles
 * 
 * Copyright (C) 2021 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>


class Transforms {

    private:

        // y = Ax + b helper for frame-of-reference conversion methods
        static void dot(double A[3][3], double x[3], double y[3])
        {
            for (uint8_t j = 0; j < 3; ++j) {
                y[j] = 0;
                for (uint8_t k = 0; k < 3; ++k) {
                    y[j] += A[j][k] * x[k];
                }
            }
        }

    public:

        static void eulerToQuaternion(const double eulerAngles[3], double quaternion[4])
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
            quaternion[0] = cph * cth * cps + sph * sth * sps;
            quaternion[1] = cph * sth * sps - sph * cth * cps;
            quaternion[2] = -cph * sth * cps - sph * cth * sps;
            quaternion[3] = cph * cth * sps - sph * sth * cps;
        }


        static void inertialToBody(double inertial[3], const double rotation[3], double body[3])
        {
            double phi = rotation[0];
            double theta = rotation[1];
            double psi = rotation[2];

            double cph = cos(phi);
            double sph = sin(phi);
            double cth = cos(theta);
            double sth = sin(theta);
            double cps = cos(psi);
            double sps = sin(psi);

            double R[3][3] = { {cps * cth,                cth * sps,                   -sth},
                {cps * sph * sth - cph * sps,  cph * cps + sph * sps * sth,  cth * sph},
                {sph * sps + cph * cps * sth,  cph * sps * sth - cps * sph,  cph * cth} };

            dot(R, inertial, body);
        }

        // bodyToInertial method optimized for body X=Y=0
        static void bodyZToInertial(double bodyZ, const double rotation[3], double inertial[3])
        {
            double phi = rotation[0];
            double theta = rotation[1];
            double psi = rotation[2];

            double cph = cos(phi);
            double sph = sin(phi);
            double cth = cos(theta);
            double sth = sin(theta);
            double cps = cos(psi);
            double sps = sin(psi);

            // This is the rightmost column of the body-to-inertial rotation matrix
            double R[3] = { sph * sps + cph * cps * sth,
                cph * sps * sth - cps * sph,
                cph * cth };

            for (uint8_t i = 0; i < 3; ++i) {
                inertial[i] = bodyZ * R[i];
            }
        }

        static void bodyToInertial(double body[3], const double rotation[3], double inertial[3])
        {
            double phi = rotation[0];
            double theta = rotation[1];
            double psi = rotation[2];

            double cph = cos(phi);
            double sph = sin(phi);
            double cth = cos(theta);
            double sth = sin(theta);
            double cps = cos(psi);
            double sps = sin(psi);

            double R[3][3] = { {cps * cth,  cps * sph * sth - cph * sps,  sph * sps + cph * cps * sth},
                {cth * sps,  cph * cps + sph * sps * sth,  cph * sps * sth - cps * sph},
                {-sth,     cth * sph,                cph * cth} };

            dot(R, body, inertial);
        }

}; // class Transforms
