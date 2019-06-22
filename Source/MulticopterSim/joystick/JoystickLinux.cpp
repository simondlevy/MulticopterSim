/*
 * JoystickLinux.cpp: Linux implementation of joystick/gamepad support for MulticopterSim
 *
 * Copyright (C) 2018 Simon D. Levy
 *
 * MIT License
 */

#ifdef __linux__

#include "Joystick.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <linux/joystick.h>
#include <string.h>
#include <unistd.h>
#include <stdio.h>

static const char * DEVNAME = "/dev/input/js0";

#ifndef debug
#define debug printf
#endif

enum {

    AXIS_THROTTLE,
    AXIS_ROLL,
    AXIS_PITCH,
    AXIS_YAW,
    AXIS_NONE
};

//                                            0         1              2          3           4
static uint8_t F310_MAP[5]             = {AXIS_YAW, AXIS_THROTTLE, AXIS_NONE, AXIS_ROLL,  AXIS_PITCH};
static uint8_t SPEKTRUM_MAP[5]         = {AXIS_YAW, AXIS_THROTTLE, AXIS_ROLL, AXIS_PITCH, AXIS_NONE};
static uint8_t XBOX360_WIRELESS_MAP[5] = {AXIS_YAW, AXIS_THROTTLE, AXIS_NONE, AXIS_ROLL,  AXIS_PITCH};

static char productName[128];

Joystick::Joystick(void) 
{
    _joystickId = open(DEVNAME, O_RDONLY);

    if (_joystickId > 0) {

        fcntl(_joystickId, F_SETFL, O_NONBLOCK);

        *productName = 0;

        if (ioctl(_joystickId, JSIOCGNAME(sizeof(productName)), productName) < 0) {
            return;
        }

        if (strstr(productName, "Taranis") || strstr(productName, "DeviationTx Deviation GamePad")) {
            _productId = PRODUCT_TARANIS;
            _isRcTransmitter = true;
        }
        else if (strstr(productName, "Horizon Hobby SPEKTRUM")) {
            _productId = PRODUCT_SPEKTRUM;
            _isRcTransmitter = true;
        }
        else if (strstr(productName, "Extreme 3D")) {
            _productId = PRODUCT_EXTREMEPRO3D;
        }
        else if (strstr(productName, "Generic X-Box pad")) {
            _productId = PRODUCT_XBOX360_CLONE;
        }
        else if (strstr(productName, "Logitech Gamepad F310")) {
            _productId = PRODUCT_F310;
        }
        else if (strstr(productName, "Xbox 360 Wireless Receiver")) {
            _productId = PRODUCT_XBOX360_WIRELESS;
        }

        // XXX should check for PS3 clone
    }
}

Joystick::error_t Joystick::poll(float axes[6], uint8_t & buttonState)
{
    if (_joystickId <= 0) return ERROR_PRODUCT;

    struct js_event js;

    read(_joystickId, &js, sizeof(struct js_event));

    if (js.type & JS_EVENT_INIT) return ERROR_MISSING;

    static float _axes[6];

    uint8_t * axisMap = NULL;

    switch (_productId) {

        case PRODUCT_F310:
            axisMap = F310_MAP;
            break;

        case PRODUCT_SPEKTRUM:
            axisMap = SPEKTRUM_MAP;
            break;

        case PRODUCT_XBOX360_WIRELESS:
            axisMap = XBOX360_WIRELESS_MAP;
            break;

        default:
            debug("JOYSTICK %s NOT RECOGNIZED", productName);
            return ERROR_PRODUCT;
    }

    switch (js.type) {

        case JS_EVENT_AXIS: 
            _axes[axisMap[js.number]] = js.value / 32768.f;
            break;

        case JS_EVENT_BUTTON:
            //_buttons = js.number + 1; // avoid zero
            break;
    }

    for (uint8_t k=0; k<6; ++k) {
        axes[k] = _axes[k];
    }

    // Invert axes 0, 2 for unless R/C transmitter
    if (!_isRcTransmitter) {
        axes[0] = -axes[0];
        axes[2] = -axes[2];
    }

    return ERROR_NOERROR;
}  

#endif
