/*
* Physics.cpp: Abstract physics class for MulticopterSim
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#include "Physics.h"

Physics::Physics(class AVehiclePawn * vehiclePawn, class UStaticMeshComponent* vehicleMesh)
{
    _vehiclePawn = vehiclePawn;
    _vehicleMesh = vehicleMesh;
}

