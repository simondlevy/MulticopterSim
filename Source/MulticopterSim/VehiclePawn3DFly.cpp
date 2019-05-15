/*
* VehiclePawn3DFly.cpp: Constants for simulated 3DFly vehicle
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

#include "VehiclePawn.h"

const uint8_t AVehiclePawn::getMotorCount(void)
{
    return 4;
}

const int8_t AVehiclePawn::getMotorDirection(uint8_t j)
{
    // Support for simulating spinning propellers
    static constexpr int8_t motordirs[4] = {+1, -1, -1, +1};

    return motordirs[j];
}

const char ** AVehiclePawn::getPropellerMeshNames(void)
{
    static const char * meshnames[4] = {"Prop1", "Prop2", "Prop3", "Prop4"};
    return meshnames;
}
