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

        double _motorvals[4];

        double motorsToAngularVelocity(uint8_t a, uint8_t b, uint8_t c, uint8_t d);

    protected:

        virtual void getForces(double & Fz, double & L, double & M, double & N) override;

    public:

        virtual void setMotors(double * motorvals) override;
};
