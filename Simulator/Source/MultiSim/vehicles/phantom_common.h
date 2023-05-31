/*
* Common declarations for DJI Phantom
*
* Copyright (C) 2023 Simon D. Levy
*
* MIT License
 */

#pragma once

#include <CoreMinimal.h>
#include <GameFramework/Pawn.h>

#include "../Vehicle.hpp"

#include "../dynamics/fixedpitch/QuadXBF.hpp"

#include "../dynamics/fixedpitch/QuadXBF.hpp"

// Structures to hold static mesh initializations
DECLARE_STATIC_MESH(FFrameStatics, "Phantom/Frame.Frame", FrameStatics)
DECLARE_STATIC_MESH(FPropCWStatics, "Phantom/PropCW.PropCW", PropCWStatics)
DECLARE_STATIC_MESH(FPropCCWStatics, "Phantom/PropCCW.PropCCW", PropCCWStatics)

static Dynamics::vehicle_params_t phantom_vparams = {

    // Estimated
    2.E-06, // d drag cofficient [T=d*w^2]

    // https://www.dji.com/phantom-4/info
    1.380,  // m mass [kg]

    // Estimated
    2,      // Ix [kg*m^2] 
    2,      // Iy [kg*m^2] 
    3,      // Iz [kg*m^2] 
    38E-04, // Jr prop inertial [kg*m^2] 
    15000,  // maxrpm

    20      // maxspeed [m/s]
};

static FixedPitchDynamics::fixed_pitch_params_t phantom_fparams = {
    5.E-06, // b thrust coefficient [F=b*w^2]
    0.350   // l arm length [m]
};
