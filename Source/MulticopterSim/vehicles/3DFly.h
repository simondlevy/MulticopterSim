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
    0.0000530216718361085, // b
    2.23656692806239E-06,  // d
    /*16.47*/ 0.1000,                 // m (kilograms) 
    /*0.6*/0.0525 ,                   // l (meters)
    2,                     // Ix
    2,                     // Iy
    3,                     // Iz
    0.000308013,           // Jr (Kg*m^2)
    5000                  // maxrpm
};

