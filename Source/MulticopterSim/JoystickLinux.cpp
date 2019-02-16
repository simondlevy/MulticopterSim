/*
 * JoystickLinux.cpp: Linux implementation of joystick/gamepad support for MulticopterSim
 *
 * Copyright (C) 2018 Simon D. Levy
 *
 * MIT License
 */

#ifdef __linux__

#include "Joystick.h"
#include "VehiclePawn.h"

#include <unistd.h>
#include <sys/time.h>
#include <linux/joystick.h>


static const char * DEVNAME = "/dev/input/js0";

Joystick::Joystick(void) 
{
    _joystickId = open(DEVNAME, O_RDONLY);

    if (_joystickId > 0) {

        fcntl(_joystickId, F_SETFL, O_NONBLOCK);

        char productName[128];

        if (ioctl(_joystickId, JSIOCGNAME(sizeof(productName)), productName) < 0) {
            return;
        }

        if (strstr(productName, "Taranis") || strstr(productName, "DeviationTx Deviation GamePad")) {
            _productId = PRODUCT_TARANIS;
            isRcTransmitter = true;
        }
        else if (strstr(productName, "Horizon Hobby SPEKTRUM")) {
            _productId = PRODUCT_TARANIS;
            isRcTransmitter = true;
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
        else { // default to PS3 clone
            _productId = PRODUCT_PS3_CLONE;
        }
    }
}

void Joystick::poll(float axes[6], uint8_t & buttonState)
{
    if (_joystickId <= 0) return;

    struct js_event js;

    read(_joystickId, &js, sizeof(struct js_event));

    if (js.type & JS_EVENT_INIT) return;

    switch (js.type) {

        case JS_EVENT_AXIS:
            //axes[_axisMap[js.number]] = js.value / 32768.f;
            break;

        case JS_EVENT_BUTTON:
            //_buttons = js.number + 1; // avoid zero
            break;
    }
}  


#endif
