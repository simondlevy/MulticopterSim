/*
* SimReceiver.cpp : Support USB controller for flight simulators
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

#include "SimReceiver.h"

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
    // Display axes (helps debug new controllers)
    //hf::Debug::printf("0:%d  1:%d  2:%d 3:%d  4:%d  5:%d", axes[0], axes[1], axes[2], axes[3], axes[4], axes[5]);

    // Normalize the axes to demands in [-1,+1]
    for (uint8_t k=0; k<5; ++k) {
        rawvals[k] = (_axes[joystick.axismap[k]] - 32767) / 32767.f;
    }

    // Invert throttle, pitch if indicated
    if (joystick.reversedVerticals) {
        rawvals[0] = -rawvals[0];
        rawvals[2] = -rawvals[2];
    }

    // For game controllers, use buttons to fake up values in a three-position aux switch
    if (joystick.useButtonForAux) {
        for (uint8_t k=0; k<3; ++k) {
            if (_buttons == joystick.buttonmap[k]) {
                _buttonState = k;
            }
        }
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
    joystick.poll(_axes, _buttons);
}
