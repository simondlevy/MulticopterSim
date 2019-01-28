/*
* SimReceiver.cpp : Support USB controller for flight simulators
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

#include "SimReceiver.h"
#include <debug.hpp>

hf::SimReceiver::SimReceiver(void)
{
    joystick.init();

    _cycle = 0;
    _buttonState = 0;
}

void hf::SimReceiver::begin(void)
{
}

bool hf::SimReceiver::gotNewFrame(void)
{
    return (++_cycle % 3) ? false : true;
}

void hf::SimReceiver::readRawvals(void)
{
    // For game controllers, use buttons to fake up values in a three-position aux switch
    if (!joystick.isRcTransmitter) {
       rawvals[4] = buttonsToAux[_buttonState];
    }
}

uint8_t hf::SimReceiver::getAux1State(void) 
{
    return Receiver::getAux1State();
}

uint8_t hf::SimReceiver::getAux2State(void)
{
    // Always armed!
    return 1;
}


void hf::SimReceiver::update(void)
{
    joystick.poll(rawvals, _buttonState);
}
