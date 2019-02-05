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
 *  Abstract class for vehicle physics
 */
class MULTICOPTERSIM_API VehiclePhysics
{
    public:

        /**
         *  Computes rotation and translation forces
         *  @param input: deltaSeconds time since last update
         *  @param input: motorValues current motor values in [0,1]
         *  @param input: eulerAngles current Euler angles in radians
         *  @param output: rotationForce as 3D vector
         *  @param output: translationForce as 3D vector 
         */
         virtual void computeForces(float deltaSeconds, TArray<float> motorValues, FVector & eulerAngles, 
                FVector & rotationalForce, FVector & translationForce) = 0;

        /**
         *  Factory method.
         *  @return pointer to a new VehiclePhysics object
         */
        static VehiclePhysics * createVehiclePhysics(void);
};


