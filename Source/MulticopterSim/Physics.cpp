/*
* Physics.cpp: Abstract physics class for MulticopterSim
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#include "Physics.h"

Physics::Physics(class AVehiclePawn * vehiclePawn)
{
    _vehiclePawn = vehiclePawn;
}

double Physics::getCurrentTime(void)
{
    return FPlatformTime::Seconds();
}

