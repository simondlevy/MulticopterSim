/*
 * Joystick.cpp: joystick/gamepad support for MulticopterSim
 *
 * Copyright (C) 2018 Simon D. Levy
 *
 * MIT License
 */


#include "Joystick.h"

#define WIN32_LEAN_AND_MEAN

#include <shlwapi.h>
#include "joystickapi.h"


void Joystick::init(void)
{
    JOYCAPS joycaps;

    // Grab the first available joystick
    for (_id=0; _id<16; _id++)
        if (joyGetDevCaps(_id, &joycaps, sizeof(joycaps)) == JOYERR_NOERROR)
            break;

    if (_id < 16) {

        uint16_t vendorId = joycaps.wMid;
        uint16_t productId = joycaps.wPid;

        // axes: 0=Thr 1=Ael 2=Ele 3=Rud 4=Aux
        // JOYINFOEX: 0=dwXpos 1=dwYpos 2=dwZpos 3=dwRpos 4=dwUpos 5=dwVpos

        // R/C transmitter
        if (vendorId == VENDOR_STM) {

            if (productId == PRODUCT_TARANIS) {
                axismap[0] =   0;
                axismap[1] =   1;
                axismap[2] =   2;
                axismap[3] =   5;
                axismap[4] =   3;
            }
            else { // Spektrum
                axismap[0] = 1;
                axismap[1] = 2;
                axismap[2] = 5;
                axismap[3] = 0;
                axismap[4] = 4;
            }
        }

        else {

            reversedVerticals = true;

            switch (productId) {

                case PRODUCT_PS3_CLONE:      
                case PRODUCT_PS4:
                    axismap[0] = 1;
                    axismap[1] = 2;
                    axismap[2] = 3;
                    axismap[3] = 0;
                    springyThrottle = true;
                    useButtonForAux = true;
                    buttonmap[0] = 1;
                    buttonmap[1] = 2;
                    buttonmap[2] = 4;
                    break;


                case PRODUCT_XBOX360_CLONE:
                    axismap[0] = 1;
                    axismap[1] = 4;
                    axismap[2] = 3;
                    axismap[3] = 0;
                    springyThrottle = true;
                    useButtonForAux = true;
                    buttonmap[0] = 8;
                    buttonmap[1] = 2;
                    buttonmap[2] = 1;
                    break;

                case PRODUCT_EXTREMEPRO3D:  
                    axismap[0] = 2;
                    axismap[1] = 0;
                    axismap[2] = 1;
                    axismap[3] = 3;
                    useButtonForAux = true;
                    buttonmap[0] = 1;
                    buttonmap[1] = 2;
                    buttonmap[2] = 4;
                    break;

                case PRODUCT_F310:
                    axismap[0] = 1;
                    axismap[1] = 4;
                    axismap[2] = 3;
                    axismap[3] = 0;
                    springyThrottle = true;
                    useButtonForAux = true;
                    buttonmap[0] = 8;
                    buttonmap[1] = 2;
                    buttonmap[2] = 1;
                    break;
            }
        }
    }
}

void Joystick::poll(void)
{
    JOYINFOEX joyState;
    joyState.dwSize=sizeof(joyState);
    joyState.dwFlags=JOY_RETURNALL | JOY_RETURNPOVCTS | JOY_RETURNCENTERED | JOY_USEDEADZONE;
    joyGetPosEx(_id, &joyState);

    int32_t axes[6];
    axes[0] = joyState.dwXpos;
    axes[1] = joyState.dwYpos;
    axes[2] = joyState.dwZpos;
    axes[3] = joyState.dwRpos;
    axes[4] = joyState.dwUpos;
    axes[5] = joyState.dwVpos;

    uint8_t buttons = joyState.dwButtons;

}

