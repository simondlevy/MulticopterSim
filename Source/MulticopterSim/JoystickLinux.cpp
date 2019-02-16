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

class LinuxJoystick : public Joystick {

    private:

        const char * DEVNAME = "/dev/input/js0";

        int _joystickId;

        uint8_t _axisMap[6];
        uint8_t _buttonMap[3];
        bool _reversedVerticals;
        bool _useButtonForAux;

    public:

        LinuxJoystick(void) 
        {
            isRcTransmitter = false;
            _reversedVerticals = false;
            _useButtonForAux = false;

            _joystickId = open(DEVNAME, O_RDONLY);

            if (_joystickId > 0) {

                fcntl(_joystickId, F_SETFL, O_NONBLOCK);

                char productName[128];

                if (ioctl(_joystickId, JSIOCGNAME(sizeof(productName)), productName) < 0) {
                    return;
                }

                if (strstr(productName, "Taranis") || strstr(productName, "DeviationTx Deviation GamePad")) {
                    _axisMap[0] = 0;
                    _axisMap[1] = 1;
                    _axisMap[2] = 2;
                    _axisMap[3] = 3;
                    isRcTransmitter = true;
                }
                else if (strstr(productName, "Horizon Hobby SPEKTRUM")) {
                    _axisMap[0] = 1;
                    _axisMap[1] = 2;
                    _axisMap[2] = 3;
                    _axisMap[3] = 0;
                    _axisMap[4] = 4;
                    isRcTransmitter = true;
                }
                else if (strstr(productName, "Extreme 3D")) {
                    _axisMap[0] = 3;
                    _axisMap[1] = 0;
                    _axisMap[2] = 1;
                    _axisMap[3] = 2;
                    _reversedVerticals = true;
                    _useButtonForAux = true;
                    _buttonMap[0] = 1;
                    _buttonMap[1] = 2;
                    _buttonMap[2] = 3;
                }
                else if (strstr(productName, "Generic X-Box pad")) {
                    _axisMap[0] =  1;
                    _axisMap[1] =  3;
                    _axisMap[2] =  4;
                    _axisMap[3] =  0;
                    _reversedVerticals = true;
                    _useButtonForAux = true;
                    _buttonMap[0] = 4;
                    _buttonMap[1] = 2;
                    _buttonMap[2] = 1;
                }
                else if (strstr(productName, "Logitech Gamepad F310")) {
                }
                else { // default to PS3
                    _axisMap[0] = 1;
                    _axisMap[1] = 3;
                    _axisMap[2] = 4;
                    _axisMap[3] = 0;
                    _reversedVerticals = true;
                    _useButtonForAux = true;
                    _buttonMap[0] = 1;
                    _buttonMap[1] = 2;
                    _buttonMap[2] = 3;
                }
            }
        }

        virtual void poll(float axes[6], uint8_t & buttonState) override
        {
            if (_joystickId <= 0) return;

            struct js_event js;

            read(_joystickId, &js, sizeof(struct js_event));

            if (js.type & JS_EVENT_INIT) return;

            switch (js.type) {

                case JS_EVENT_AXIS:
                    //axes[js.number] = js.value;
                    break;

                case JS_EVENT_BUTTON:
                    //_buttons = js.number + 1; // avoid zero
                    break;
            }
        }
};

Joystick * Joystick::createJoystick(void)
{
    return new LinuxJoystick();
}

#endif
