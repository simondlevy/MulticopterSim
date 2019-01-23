/*
 * HackflightSimReceiverWindows.h : Windows support for USB controllers
 *
 * Copyright (C) 2018 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "HackflightSimReceiver.h"

#include <shlwapi.h>
#pragma comment(lib, "Shlwapi.lib")

#include "joystickapi.h"

static const uint16_t VENDOR_STM	        = 0x0483;

static const uint16_t PRODUCT_TARANIS		= 0x5710;
static const uint16_t PRODUCT_PS3_CLONE		= 0x0003;
static const uint16_t PRODUCT_XBOX360_CLONE	= 0xfafe;
static const uint16_t PRODUCT_EXTREMEPRO3D	= 0xc215;
static const uint16_t PRODUCT_F310	        = 0xc21d;
static const uint16_t PRODUCT_PS4	        = 0x09cc;

void hf::SimReceiver::productInit(void)
{
    JOYCAPS joycaps;

    // Grab the first available joystick
    for (_joyid=0; _joyid<16; _joyid++)
        if (joyGetDevCaps(_joyid, &joycaps, sizeof(joycaps)) == JOYERR_NOERROR)
            break;

    if (_joyid < 16) {

		uint16_t vendorId = joycaps.wMid;
		uint16_t productId = joycaps.wPid;

        // axes: 0=Thr 1=Ael 2=Ele 3=Rud 4=Aux
        // JOYINFOEX: 0=dwXpos 1=dwYpos 2=dwZpos 3=dwRpos 4=dwUpos 5=dwVpos

        // R/C transmitter
        if (vendorId == VENDOR_STM) {

            if (productId == PRODUCT_TARANIS) {
                _axismap[0] =   0;
                _axismap[1] =   1;
                _axismap[2] =   2;
                _axismap[3] =   5;
                _axismap[4] =   3;
            }
            else { // Spektrum
                _axismap[0] = 1;
                _axismap[1] = 2;
                _axismap[2] = 5;
                _axismap[3] = 0;
                _axismap[4] = 4;
            }
        }

        else {

            _reversedVerticals = true;

            switch (productId) {

                case PRODUCT_PS3_CLONE:      
                case PRODUCT_PS4:
                    _axismap[0] = 1;
                    _axismap[1] = 2;
                    _axismap[2] = 3;
                    _axismap[3] = 0;
                    _springyThrottle = true;
                    _useButtonForAux = true;
                    _buttonmap[0] = 1;
                    _buttonmap[1] = 2;
                    _buttonmap[2] = 4;
                    break;


                case PRODUCT_XBOX360_CLONE:
                    _axismap[0] = 1;
                    _axismap[1] = 4;
                    _axismap[2] = 3;
                    _axismap[3] = 0;
                    _springyThrottle = true;
                    _useButtonForAux = true;
                    _buttonmap[0] = 8;
                    _buttonmap[1] = 2;
                    _buttonmap[2] = 1;
                    break;

                case PRODUCT_EXTREMEPRO3D:  
                    _axismap[0] = 2;
                    _axismap[1] = 0;
                    _axismap[2] = 1;
                    _axismap[3] = 3;
                    _useButtonForAux = true;
                    _buttonmap[0] = 1;
                    _buttonmap[1] = 2;
                    _buttonmap[2] = 4;
                    break;

                case PRODUCT_F310:
                    _axismap[0] = 1;
                    _axismap[1] = 4;
                    _axismap[2] = 3;
                    _axismap[3] = 0;
                    _useButtonForAux = true;
                    _buttonmap[0] = 8;
                    _buttonmap[1] = 2;
                    _buttonmap[2] = 1;
                    break;
            }

        }
    }
}

void hf::SimReceiver::productPoll(int32_t axes[6], uint8_t & buttons)
{
	JOYINFOEX joyState;
    joyState.dwSize=sizeof(joyState);
    joyState.dwFlags=JOY_RETURNALL | JOY_RETURNPOVCTS | JOY_RETURNCENTERED | JOY_USEDEADZONE;
    joyGetPosEx(_joyid, &joyState);

    axes[0] = joyState.dwXpos;
    axes[1] = joyState.dwYpos;
    axes[2] = joyState.dwZpos;
    axes[3] = joyState.dwRpos;
    axes[4] = joyState.dwUpos;
    axes[5] = joyState.dwVpos;

    buttons = joyState.dwButtons;
}

int32_t hf::SimReceiver::productGetBaseline(void)
{
    return 32767;
}
