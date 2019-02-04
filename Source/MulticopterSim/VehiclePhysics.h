/*
* VehiclePhysics.h: Class declaration for abstract physics class in MulticopterSim
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#pragma once

#include "CoreMinimal.h"

/**
 * 
 */
class MULTICOPTERSIM_API VehiclePhysics
{
    public:

        virtual void computeAngularForces(float motors[4], float rotationalForces[3], float & overallThrust) = 0;

        /**
         *  Factory method.
         */
        static VehiclePhysics * createVehiclePhysics(void);
};


