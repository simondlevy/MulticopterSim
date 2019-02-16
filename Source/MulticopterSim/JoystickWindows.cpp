/*
 * JoystickWindows.cpp: Windows implementation of joystick/gamepad support for MulticopterSim
 *
 * Copyright (C) 2018 Simon D. Levy
 *
 * MIT License
 */

#ifdef _WIN32

#include "Joystick.h"

#include "VehiclePawn.h"

#define WIN32_LEAN_AND_MEAN

#include <shlwapi.h>
#include "joystickapi.h"

void Joystick::init(void)
{
    JOYCAPS joycaps = {0};

    _productId = 0;

    isRcTransmitter = false;

    // Grab the first available joystick
    for (_joystickId=0; _joystickId<16; _joystickId++)
        if (joyGetDevCaps(_joystickId, &joycaps, sizeof(joycaps)) == JOYERR_NOERROR)
            break;

    if (_joystickId < 16) {

        _productId = joycaps.wPid;

        isRcTransmitter = (joycaps.wMid == VENDOR_STM);
    }
}

void Joystick::poll(float axes[6], uint8_t & buttonState)
{
    JOYINFOEX joyState;
    joyState.dwSize=sizeof(joyState);
    joyState.dwFlags=JOY_RETURNALL | JOY_RETURNPOVCTS | JOY_RETURNCENTERED | JOY_USEDEADZONE;
    joyGetPosEx(_joystickId, &joyState);

    // axes: 0=Thr 1=Ael 2=Ele 3=Rud 4=Aux

    // R/C transmitter
    if (isRcTransmitter) {

        if (_productId == PRODUCT_TARANIS) {
            getAxes(axes, joyState.dwXpos, joyState.dwYpos, joyState.dwZpos, joyState.dwVpos, joyState.dwRpos);
        }
        else { // Spektrum
            getAxes(axes, joyState.dwYpos, joyState.dwZpos, joyState.dwVpos, joyState.dwXpos, joyState.dwUpos);
        }
    }

    else {

        switch (_productId) {

            case PRODUCT_PS3_CLONE:      
            case PRODUCT_PS4:
                getAxes(axes, joyState.dwYpos, joyState.dwZpos, joyState.dwRpos, joyState.dwXpos, 0);
                getButtons(joyState.dwButtons, buttonState, 1, 2, 4);
                break;


            case PRODUCT_XBOX360:  
            case PRODUCT_XBOX360_CLONE:
            case PRODUCT_F310:
                getAxes(axes, joyState.dwYpos, joyState.dwUpos, joyState.dwRpos, joyState.dwXpos, 0);
                getButtons(joyState.dwButtons, buttonState, 8, 2, 1);
                break;

            case PRODUCT_EXTREMEPRO3D:  
                getAxes(axes, joyState.dwZpos, joyState.dwXpos, joyState.dwYpos, joyState.dwRpos, 0);
                getButtons(joyState.dwButtons, buttonState, 1, 2, 4);
                break;
        }
    }

    // Normalize the axes to demands in [-1,+1]
    for (uint8_t k=0; k<5; ++k) {
        axes[k] = (axes[k] - 32767) / 32767.f;
    }
    
    // Invert axes 0, 2 for unless R/C transmitter
    if (!isRcTransmitter) {
        axes[0] = -axes[0];
        axes[2] = -axes[2];
    }
}

void Joystick::getAxes(float axes[6], DWORD axis0, DWORD axis1, DWORD axis2, DWORD axis3, DWORD axis4)
{
    axes[0] = axis0;
    axes[1] = axis1;
    axes[2] = axis2;
    axes[3] = axis3;
    axes[4] = axis4;
}

void Joystick::getButtons(DWORD dwButtons, uint8_t & buttonState, uint8_t button0, uint8_t button1, uint8_t button2)
{
    if (dwButtons == button0) {
        buttonState = 0;
    }
    else if (dwButtons == button1) {
        buttonState = 1;
    }
    else if (dwButtons == button2) {
        buttonState = 2;
    }
}

#endif
