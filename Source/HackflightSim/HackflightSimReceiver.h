/*
* HackflightSimReceiver.h : Support USB controller for flight simulators
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

#pragma once

#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <math.h>
#include <fcntl.h>

#include <receiver.hpp>

#include "joystickapi.h"

static const uint16_t VENDOR_STM	        = 0x0483;

static const uint16_t PRODUCT_TARANIS		= 0x5710;
static const uint16_t PRODUCT_PS3_CLONE		= 0x0003;
static const uint16_t PRODUCT_XBOX360_CLONE	= 0xfafe;
static const uint16_t PRODUCT_EXTREMEPRO3D	= 0xc215;
static const uint16_t PRODUCT_F310	        = 0xc21d;
static const uint16_t PRODUCT_PS4	        = 0x09cc;

namespace hf {

    class SimReceiver : public Receiver {

        public:

            SimReceiver(void) 
            {
                _reversedVerticals = false;
                _springyThrottle = false;
                _useButtonForAux = false;
                _joyid = 0;
                _cycle = 0;

                _buttonState = 0;
            }

            void begin(void)
            {
                // Set up axes based on OS and controller
                productInit();
            }

            bool gotNewFrame(void)
            {
                return (++_cycle % 3) ? false : true;
            }

            void readRawvals(void)
            {
                static int32_t axes[6];
                static uint8_t buttons;

                // Grab the axis values in an OS-specific way
                productPoll(axes, buttons);

                // Display axes (helps debug new controllers)
                //hf::Debug::printf("0:%d  1:%d  2:%d 3:%d  4:%d  5:%d", axes[0], axes[1], axes[2], axes[3], axes[4], axes[5]);

                // Normalize the axes to demands in [-1,+1]
                for (uint8_t k=0; k<5; ++k) {
                    rawvals[k] = (axes[_axismap[k]] - productGetBaseline()) / 32767.f;
                }

                // Invert throttle, pitch if indicated
                if (_reversedVerticals) {
                    rawvals[0] = -rawvals[0];
                    rawvals[2] = -rawvals[2];
                }

                // For game controllers, use buttons to fake up values in a three-position aux switch
                if (_useButtonForAux) {
                    for (uint8_t k=0; k<3; ++k) {
                        if (buttons == _buttonmap[k]) {
                            _buttonState = k;
                        }
                    }
                    rawvals[4] = buttonsToAux[_buttonState];
                }
            }

            void halt(void)
            {
            }

        protected:

            virtual uint8_t getAux1State(void) override
            {
                return _springyThrottle ? 2 : Receiver::getAux1State();
            }

            virtual uint8_t getAux2State(void) override
            {
                return 1; // always armed!
            }

        private:

            // Determined dynamically based on controller
            bool     _reversedVerticals;
            bool     _springyThrottle;
            bool     _useButtonForAux;
            uint8_t  _axismap[5];   // Thr, Ael, Ele, Rud, Aux
            uint8_t  _buttonmap[3]; // Aux=0, Aux=1, Aux=2
            int      _joyid;        // Linux file descriptor or Windows joystick ID

            // Simulate auxiliary switch via pushbuttons
            uint8_t _buttonState;
            const float buttonsToAux[3] = {-.1f, 0.f, .8f};

            // Helps mock up periodic availability of new data frame (output data rate; ODR)
            uint64_t _cycle;          

            void productInit(void)
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

            void productPoll(int32_t axes[6], uint8_t & buttons)
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

            int32_t productGetBaseline(void)
            {
                return 32767;
            }

    }; // class SimReceiver

} // namespace hf
