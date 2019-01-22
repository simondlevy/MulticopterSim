/*
* HackflightSimController.cpp: Abstract flight-control class for HackflightSim
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#include "HackflightSimController.h"

HackflightSimController::HackflightSimController(void)
{
}

HackflightSimController::~HackflightSimController(void)
{
}

void HackflightSimController::update(void)
{
}

HackflightSimController * HackflightSimController::createController(void)
{
    return new HackflightSimController();
}
