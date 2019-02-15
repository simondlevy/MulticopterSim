/*
 * JoystickLinux.cpp: Linux implementation of joystick/gamepad support for MulticopterSim
 *
 * Copyright (C) 2018 Simon D. Levy
 *
 * MIT License
 */

#include "Joystick.h"
#include "VehiclePawn.h"

void Joystick::init(void)
{
    _productId = 0;

    isRcTransmitter = false;
}

void Joystick::poll(float axes[6], uint8_t & buttonState)
{
}

void Joystick::getAxes(float axes[6], DWORD axis0, DWORD axis1, DWORD axis2, DWORD axis3, DWORD axis4)
{
}

void Joystick::getButtons(DWORD dwButtons, uint8_t & buttonState, uint8_t button0, uint8_t button1, uint8_t button2)
{
}
