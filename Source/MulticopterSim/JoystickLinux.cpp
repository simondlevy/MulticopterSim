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

/*
#include <unistd.h>
#include <sys/time.h>
#include <linux/joystick.h>

static const char * DEVNAME = "/dev/input/js0";

void hf::Controller::productInit(void)
{
    if ((_joyid=open(DEVNAME, O_RDONLY)) > 0) {

        fcntl(_joyid, F_SETFL, O_NONBLOCK);

        char prodname[128];

        if (ioctl(_joyid, JSIOCGNAME(sizeof(prodname)), prodname) < 0) {
            return;
        }

        if (strstr(prodname, "Taranis") || strstr(prodname, "DeviationTx Deviation GamePad")) {
            _axismap[0] = 0;
            _axismap[1] = 1;
            _axismap[2] = 2;
            _axismap[3] = 3;
            _axismap[4] = 5; // We have to skip to this channel to support Aux on Windows
        }
        else if (strstr(prodname, "Horizon Hobby SPEKTRUM")) {
            _axismap[0] = 1;
            _axismap[1] = 2;
            _axismap[2] = 3;
            _axismap[3] = 0;
            _axismap[4] = 4;
        }
        else if (strstr(prodname, "Extreme 3D")) {
            _axismap[0] = 3;
            _axismap[1] = 0;
            _axismap[2] = 1;
            _axismap[3] = 2;
            _reversedVerticals = true;
            _useButtonForAux = true;
            _buttonmap[0] = 1;
            _buttonmap[1] = 2;
            _buttonmap[2] = 3;
        }
        else if (strstr(prodname, "Generic X-Box pad")) {
            _axismap[0] =  1;
            _axismap[1] =  3;
            _axismap[2] =  4;
            _axismap[3] =  0;
            _reversedVerticals = true;
            _springyThrottle = true;
            _useButtonForAux = true;
            _buttonmap[0] = 4;
            _buttonmap[1] = 2;
            _buttonmap[2] = 1;
        }
        else { // default to PS3
            _axismap[0] = 1;
            _axismap[1] = 2;
            _axismap[2] = 3;
            _axismap[3] = 0;
            _reversedVerticals = true;
            _springyThrottle = true;
            _useButtonForAux = true;
            _buttonmap[0] = 1;
            _buttonmap[1] = 2;
            _buttonmap[2] = 3;
        }
    }
}

void hf::Controller::productPoll(int32_t axes[6], uint8_t & buttons)
{
    if (_joyid <= 0) return;

    struct js_event js;

    read(_joyid, &js, sizeof(struct js_event));

    if (js.type & JS_EVENT_INIT) return;

    switch (js.type) {
        case JS_EVENT_AXIS:
            axes[js.number] = js.value;
            break;
        case JS_EVENT_BUTTON:
            buttons = js.number + 1; // avoid zero
    }
}

int32_t hf::Controller::productGetBaseline(void)
{
    return 0;
}
*/

#endif
