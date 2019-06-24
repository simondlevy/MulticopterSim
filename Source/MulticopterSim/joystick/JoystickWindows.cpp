/*
 * Windows implementation of joystick/gamepad support for flight controllers
 *
 * Copyright (C) 2018 Simon D. Levy
 *
 * MIT License
 */

#ifdef _WIN32

#include "Joystick.h"

#define WIN32_LEAN_AND_MEAN

#undef TEXT
#include <shlwapi.h>
#include "joystickapi.h"

static void getAxes(float axes[6], DWORD axis0, DWORD axis1, DWORD axis2, DWORD axis3, DWORD axis4)
{
    axes[0] = (float)axis0;
    axes[1] = (float)axis1;
    axes[2] = (float)axis2;
    axes[3] = (float)axis3;
    axes[4] = (float)axis4;
}

Joystick::Joystick(const char * devname)
{
    JOYCAPS joycaps = {0};

    _productId = 0;

    _isRcTransmitter = false;

    // Grab the first available joystick
    for (_joystickId=0; _joystickId<16; _joystickId++)
        if (joyGetDevCaps(_joystickId, &joycaps, sizeof(joycaps)) == JOYERR_NOERROR)
            break;

    if (_joystickId < 16) {

        _productId = joycaps.wPid;

        _isRcTransmitter = (_productId == PRODUCT_TARANIS || _productId == PRODUCT_SPEKTRUM);
    }
}

Joystick::error_t Joystick::pollProduct(float axes[6])
{
    JOYINFOEX joyState;
    joyState.dwSize=sizeof(joyState);
    joyState.dwFlags=JOY_RETURNALL | JOY_RETURNPOVCTS | JOY_RETURNCENTERED | JOY_USEDEADZONE;
    joyGetPosEx(_joystickId, &joyState);

    // axes: 0=Thr 1=Ael 2=Ele 3=Rud 4=Aux

    switch (_productId) {

        case PRODUCT_SPEKTRUM:
            getAxes(axes, joyState.dwYpos, joyState.dwZpos, joyState.dwVpos, joyState.dwXpos, joyState.dwUpos);
            break;

        case PRODUCT_TARANIS:
            getAxes(axes, joyState.dwXpos, joyState.dwYpos, joyState.dwZpos, joyState.dwVpos, joyState.dwRpos);
            break;

        case PRODUCT_PS3_CLONE:      
        case PRODUCT_PS4:
            getAxes(axes, joyState.dwYpos, joyState.dwZpos, joyState.dwRpos, joyState.dwXpos, 0);
            break;


        case PRODUCT_F310:
            getAxes(axes, joyState.dwYpos, joyState.dwZpos, joyState.dwRpos, joyState.dwXpos, 0);
            break;

        case PRODUCT_XBOX360:  
        case PRODUCT_XBOX360_CLONE:
        case PRODUCT_XBOX360_CLONE2:
            getAxes(axes, joyState.dwYpos, joyState.dwUpos, joyState.dwRpos, joyState.dwXpos, 0);
            break;

        case PRODUCT_EXTREMEPRO3D:  
            getAxes(axes, joyState.dwZpos, joyState.dwXpos, joyState.dwYpos, joyState.dwRpos, 0);
            break;

        case PRODUCT_INTERLINK:

            getAxes(axes, joyState.dwZpos, joyState.dwXpos, joyState.dwYpos, joyState.dwRpos, 0);

            // rescale axes (should be done in RealFlight!)
            rescaleAxis(axes[0], 13161, 51336);                          
            rescaleAxis(axes[1], 12623, 55342);
            rescaleAxis(axes[2], 13698, 51335);
            rescaleAxis(axes[3], 11818, 55159);

            //getButtons(joyState.dwButtons&0xFE, buttonState, 10, 2, 18); // use other bits for aux state

            break;

        default:

            return _productId ? ERROR_PRODUCT : ERROR_MISSING;
    }

    // Normalize the axes to demands to [-1,+1]
    for (uint8_t k=0; k<5; ++k) {
        axes[k] = axes[k] / 32767 - 1;
    }

    return Joystick::ERROR_NOERROR;
}

void Joystick::rescaleAxis(float & value, float minval, float maxval)
{
    if (value <= 32767) {
        value = (value - minval) / (1 - minval/32767);
    }
    else {
        value = 32767 *((value - 32767) / (maxval - 32767)  + 1);
    }
}

#endif
