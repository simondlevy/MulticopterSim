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
    _productId = 0;

    isRcTransmitter = false;

    if ((_joystickId=open(DEVNAME, O_RDONLY)) > 0) {

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
            switch (_productId) {
                case PRODUCT_F310:// 1,3,4,0
                    break;
            }
            axes[js.number] = js.value;
            break;

        case JS_EVENT_BUTTON:
            _buttons = js.number + 1; // avoid zero
    }
}

void Joystick::getAxes(float axes[6], DWORD axis0, DWORD axis1, DWORD axis2, DWORD axis3, DWORD axis4)
{
}

void Joystick::getButtons(DWORD dwButtons, uint8_t & buttonState, uint8_t button0, uint8_t button1, uint8_t button2)
{
}

Joystick * Joystick::createJoystick(void)
{
    return new Joystick();
}

/*
   int32_t hf::Controller::productGetBaseline(void)
   {
       return 0;
   }
 */

#endif
