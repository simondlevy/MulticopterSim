/*
* SimReceiver.cpp : Support USB controller for flight simulators
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/


#include "SimReceiver.h"
#include <debug.hpp>

SimReceiver::SimReceiver(void)
{
}

bool SimReceiver::gotNewFrame(void) 
{
    return false;
}

void SimReceiver::readRawvals(void) 
{
}
