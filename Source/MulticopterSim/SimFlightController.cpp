/*
* SimFlightController.cpp: Abstract flight-control class for MulticopterSim
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#include "SimFlightController.h"

// Math support
#define _USE_MATH_DEFINES
#include <math.h>

#include <hackflight.hpp>
using namespace hf;
#include "SimReceiver.h"

// Main firmware
static hf::Hackflight hackflight;

SimFlightController::SimFlightController(void)
{
}

SimFlightController::~SimFlightController(void)
{
}

void SimFlightController::update(void)
{
}

SimFlightController * SimFlightController::createSimFlightController(void)
{
    return new SimFlightController();
}
