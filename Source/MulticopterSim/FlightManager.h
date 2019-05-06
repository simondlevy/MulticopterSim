/*
 * FlightManager.h: Abstract flight-management class for MulticopterSim
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "VehiclePawn.h"

class MULTICOPTERSIM_API FlightManager
{
    public:

        virtual TArray<float> update(float currentTime, FQuat quat, FVector gyro)  = 0;

        /**
         *  Factory method.
         *  @return pointer to a new FlightManager object
         */
        static FlightManager * createFlightManager(AVehiclePawn * vehiclePawn);
};
