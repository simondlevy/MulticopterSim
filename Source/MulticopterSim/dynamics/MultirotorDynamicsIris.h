/*
 * Class declaration for multirotor dynamics on 3DR Iris
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "MultirotorDynamics.h"

class IrisDynamics : public MultirotorDynamics {

    private:

        double motorsToAngularVelocity(double motorvals[4], uint8_t a, uint8_t b, uint8_t c, uint8_t d);

    protected:

        void motorsToForces(double * motorvals, double & Fz, double & L, double & M, double & N);
};
