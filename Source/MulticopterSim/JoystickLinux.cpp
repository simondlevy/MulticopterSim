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

void Joystick::init(void)
{
    _productId = 0;

    isRcTransmitter = false;

    if ((_joystickId=open(DEVNAME, O_RDONLY)) > 0) {

        fcntl(_joystickId, F_SETFL, O_NONBLOCK);

        char prodname[128];

        if (ioctl(_joystickId, JSIOCGNAME(sizeof(prodname)), prodname) < 0) {
            return;
        }

        if (strstr(prodname, "Taranis") || strstr(prodname, "DeviationTx Deviation GamePad")) {
        }
        else if (strstr(prodname, "Horizon Hobby SPEKTRUM")) {
        }
        else if (strstr(prodname, "Extreme 3D")) {
        }
        else if (strstr(prodname, "Generic X-Box pad")) {
        }
    }
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


void hf::Controller::productInit(void)
{
}

void hf::Controller::productPoll(int32_t axes[6], uint8_t & buttons)
{
    if (_joystickId <= 0) return;

    struct js_event js;

    read(_joystickId, &js, sizeof(struct js_event));

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
