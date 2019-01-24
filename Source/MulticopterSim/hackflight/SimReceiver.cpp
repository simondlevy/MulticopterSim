/*
* SimReceiver.cpp : Support USB controller for flight simulators
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

#include "SimReceiver.h"

hf::SimReceiver::SimReceiver(uint8_t axismap[5], uint8_t buttonmap[3], bool reversedVerticals, bool springyThrottle, bool useButtonForAux)
{
    memcpy(_axismap, axismap, 5);
    memcpy(_buttonmap, buttonmap, 3);

    _reversedVerticals = reversedVerticals;
    _springyThrottle = springyThrottle;
    _useButtonForAux = useButtonForAux;

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
        rawvals[k] = (_axes[_axismap[k]] - 32767) / 32767.f;
    }

    // Invert throttle, pitch if indicated
    if (_reversedVerticals) {
        rawvals[0] = -rawvals[0];
        rawvals[2] = -rawvals[2];
    }

    // For game controllers, use buttons to fake up values in a three-position aux switch
    if (_useButtonForAux) {
        for (uint8_t k=0; k<3; ++k) {
            if (_buttons == _buttonmap[k]) {
                _buttonState = k;
            }
        }
        rawvals[4] = buttonsToAux[_buttonState];
    }
}

void hf::SimReceiver::halt(void)
{
}


uint8_t hf::SimReceiver::getAux1State(void) 
{
    return _springyThrottle ? 2 : Receiver::getAux1State();
}

uint8_t hf::SimReceiver::getAux2State(void)
{
    return 1; // always armed!
}


void hf::SimReceiver::update(int32_t axes[6], uint8_t buttons)
{
    memcpy(_axes, axes, 6*sizeof(int32_t));
    _buttons = buttons;
}
