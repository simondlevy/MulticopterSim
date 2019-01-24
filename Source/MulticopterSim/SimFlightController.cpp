/*
* SimFlightController.cpp: Abstract flight-control class for MulticopterSim
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#include "SimFlightController.h"

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
