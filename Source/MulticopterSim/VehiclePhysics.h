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

        virtual void computeForces(float deltaSeconds, TArray<float> motorValues, FVector & eulerAngles, 
                FVector & rotationalForce, FVector & translationForce) = 0;

        /**
         *  Factory method.
         */
        static VehiclePhysics * createVehiclePhysics(void);
};


