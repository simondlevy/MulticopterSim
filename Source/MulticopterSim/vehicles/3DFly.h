/*
* Data structures for for simulating 3DFly vehicle
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

#pragma once

#include "dynamics/MultirotorDynamics.hpp"
#include "dynamics/QuadXAP.hpp"

static const int8_t    MOTOR_DIRECTIONS[4] = {+1, -1, -1, +1};

static const char *    PROPELLER_MESH_NAMES[4] = {"Prop1", "Prop2", "Prop3", "Prop4"};

static const wchar_t * VEHICLE_MESH_NAME = TEXT("/Game/Flying/Meshes/3DFly.3DFly");

static MultirotorDynamics::params_t params =
{
    // NB: these values are estimated / reverse-engineered for reasonable flight characteristics, not
    // determined scientifically.
    5E-08,  // b
    2E-06,  // d
    0.1000, // m (kilograms) 
    0.0525, // l (meters)
    .2,     // Ix
    .2,     // Iy
    .3,     // Iz
    3E-4,   // Jr (kg*m^2)
    50000   // maxrpm
};

