/*
 * Windows support for joysticks and other game controllers
 *
 * Copyright (C) 2021 Simon D. Levy
 *
 * MIT License
 */


#include "Joystick.h"
#include "../../MainModule/Utils.hpp"

#define WIN32_LEAN_AND_MEAN

#undef TEXT
#include <shlwapi.h>
#include "joystickapi.h"



IJoystick::IJoystick(void)
{
    JOYCAPS joycaps = {};

    _productId = 0;

    _isGameController = false;

    // Grab the first available joystick
    for (_joystickId=0; _joystickId<16; _joystickId++)
        if (joyGetDevCaps(_joystickId, &joycaps, sizeof(joycaps)) == JOYERR_NOERROR)
            break;

    if (_joystickId < 16) {

        _productId = joycaps.wPid;

        switch (_productId) {
            case PRODUCT_TARANIS_QX7:
            case PRODUCT_TARANIS_X9D:
            case PRODUCT_SPEKTRUM:
                _isGameController = false;
                break;
            default: 
                _isGameController = true;
        }

    }
}

uint16_t IJoystick::pollProduct(float axes[6], uint8_t & buttons)
{   
    JOYINFOEX joyState;
    joyState.dwSize=sizeof(joyState);
    joyState.dwFlags=JOY_RETURNALL | JOY_RETURNPOVCTS | JOY_RETURNCENTERED | JOY_USEDEADZONE;
    joyGetPosEx(_joystickId, &joyState);

    // axes: 0=Thr 1=Ael 2=Ele 3=Rud 4=Aux

    uint8_t naxes = 4;

    switch (_productId) {

        case PRODUCT_SPEKTRUM:
            getAxes5(axes, naxes, joyState.dwYpos, joyState.dwZpos, joyState.dwVpos, joyState.dwXpos, joyState.dwUpos);
            break;

        case PRODUCT_TARANIS_QX7:
        case PRODUCT_TARANIS_X9D:
            getAxes5(axes, naxes, joyState.dwXpos, joyState.dwYpos, joyState.dwZpos, joyState.dwVpos, joyState.dwRpos);
            break;

        case PRODUCT_PS3_CLONE:      
        case PRODUCT_PS4:
            getAxes4(axes, joyState.dwYpos, joyState.dwZpos, joyState.dwRpos, joyState.dwXpos);
            break;

        case PRODUCT_F310:
            getAxes4(axes, joyState.dwYpos, joyState.dwZpos, joyState.dwRpos, joyState.dwXpos);
            break;

        case PRODUCT_XBOX_ONE:
        case PRODUCT_XBOX360:
        case PRODUCT_XBOX360_CLONE:
        case PRODUCT_XBOX360_CLONE2:
        case PRODUCT_XBOX360_WIRELESS:
            getAxes4(axes, joyState.dwYpos, joyState.dwUpos, joyState.dwRpos, joyState.dwXpos);
            break;

        case PRODUCT_EXTREMEPRO3D:  
            getAxes4(axes, joyState.dwZpos, joyState.dwXpos, joyState.dwYpos, joyState.dwRpos);
            break;

        case PRODUCT_INTERLINK:
            getAxes4(axes, joyState.dwZpos, joyState.dwXpos, joyState.dwYpos, joyState.dwRpos);
            getAuxInterlink(axes, (uint8_t)joyState.dwButtons, AX_AU1, AX_AU2, AUX1_MID);
            break;

        default:
            return _productId ? _productId : 0x0001;
    }

    // Normalize the axes to demands to [-1,+1]
    for (uint8_t k=0; k<naxes; ++k) {
        axes[k] = axes[k] / 32767 - 1;
    }

    if (_productId == PRODUCT_INTERLINK) {
        adjustAxesInterlink(axes);
    }

    buttons = (uint8_t)joyState.dwButtons;

    return 0x0000; // no error
}

