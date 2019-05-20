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

    protected:

        virtual void getForces(double & Fz, double & L, double & M, double & N) override;

    public:

        virtual void setMotors(double * motorvals) override;
};
