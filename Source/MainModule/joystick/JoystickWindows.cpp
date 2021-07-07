/*
 * Windows support for joysticks and other game controllers
 *
 * Copyright (C) 2021 Simon D. Levy
 *
 * MIT License
 */


#ifdef _WIN32

#include "MainModule/joystick/Joystick.h"
#include "../Utils.hpp"

#define WIN32_LEAN_AND_MEAN

#undef TEXT
#include <shlwapi.h>
#include "joystickapi.h"

static void getAxes4(float axes[6], DWORD axis0, DWORD axis1, DWORD axis2, DWORD axis3)
{
	axes[0] = (float)axis0;
	axes[1] = (float)axis1;
	axes[2] = (float)axis2;
	axes[3] = (float)axis3;
}

static void getAxes5(float axes[6], uint8_t & naxes, DWORD axis0, DWORD axis1, DWORD axis2, DWORD axis3, DWORD axis4)
{
    naxes = 5;
	getAxes4(axes, axis0, axis1, axis2, axis3);
    axes[4] = (float)axis4;
}

// XXX Should use a separate calibration program
static void adjustAxesInterlink(float * axes)
{
    axes[0] /= 0.575f;
    axes[1] /= 0.65f;
    axes[2] /= 0.58f;
    axes[3] /= 0.65f;
}


IJoystick::IJoystick(const char * devname)
{
    (void)devname;

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

// Convert InterLink aux switches to unique gamepad buttons
static void getAuxInterlink(float * axes, uint8_t buttons, uint8_t aux1, uint8_t aux2, float auxMid)
{
	axes[aux1] = -1;
	axes[aux2] = (buttons & 0x01) ? -1.f : +1.f;

	switch (buttons) {

	case 3:
	case 2:
		axes[aux1] = auxMid;
		break;

	case 19:
	case 18:
		axes[aux1] = 1;
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

#endif
