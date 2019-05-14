/*
 * CustomPhysics.h: Physics class using custom physics/dynamics model
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "Physics.h"
#include "VehiclePawn.h"
#include "FlightManager.h"
#include "dynamics/MultirotorDynamics.h"

// Math support
#define _USE_MATH_DEFINES
#include <math.h>


class MULTICOPTERSIM_API CustomPhysics : public Physics {

    private:

        // Abstract class for flight control
        FlightManager * _flightManager;

        // Custom multirotory dynamics
        MultirotorDynamics _dynamics;

    public:

        CustomPhysics(class AVehiclePawn * vehiclePawn);

        virtual void start(void) override;

        virtual TArray<float> update(float deltaSeconds) override;

};
