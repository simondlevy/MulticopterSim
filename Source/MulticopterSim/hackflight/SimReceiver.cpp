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
    _joystick = new Joystick();

    _cycle = 0;
    _buttonState = 0;
}

void SimReceiver::begin(void)
{
}

bool SimReceiver::gotNewFrame(void)
{
    return (++_cycle % 3) ? false : true;
}

void SimReceiver::readRawvals(void)
{
    // For game controllers, use buttons to fake up values in a three-position aux switch
    if (!_joystick->isRcTransmitter) {
       rawvals[4] = buttonsToAux[_buttonState];
    }
}

uint8_t SimReceiver::getAux1State(void) 
{
    return Receiver::getAux1State();
}

uint8_t SimReceiver::getAux2State(void)
{
    // Always armed!
    return 1;
}


void SimReceiver::update(void)
{
    _joystick->poll(rawvals, _buttonState);
}
