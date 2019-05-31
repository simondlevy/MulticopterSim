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

static const wchar_t * VEHICLE_MESH_NAME = TEXT("/Game/Flying/Meshes/BigQuad/BigQuad.BigQuad"); 

static MultirotorDynamics::params_t params =
{
    
    5.30216718361085E-05, // b
    2.23656692806239E-06, // d
    16.47,                // m (kilograms) 
    0.6,                  // l (meters)
    2,                    // Ix
    2,                    // Iy
    3,                    // Iz
    3.08013E-04,          // Jr (Kg*m^2)
    15000                 // maxrpm
};

